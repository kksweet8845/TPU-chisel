package tpu



import chisel3._
import chisel3.util._
import arithmetic._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{prefix, noPrefix}

import chisel3.experimental.VecLiterals._
import chisel3.experimental.ChiselEnum




class Accumulator[T <: Data](
    inputType:          T,
    outputType:         T,
    accType:            T,
    rows:               Int,
    columns:            Int,
    df:                 Dataflow.Value
)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {


    val io = IO(new Bundle {
        val in_psum     = Input(Vec(columns, outputType))
        val in_last     = Input(Vec(columns, Bool()))
        val in_valid    = Input(Vec(columns, Bool()))
        val in_control  = Input(Vec(columns, new PEControl(accType)))        
        
        val out_sum     = Output(Vec(columns, Valid(accType)))
    })


    object Fsm extends ChiselEnum {
        val Init, Acc, Output = Value
    }

    // val state = RegInit(Fsm.Acc)

    val (sr_psums, sr_lasts, sr_valids) = Seq.tabulate(columns) { i => 

        val state = RegInit(Fsm.Init).suggestName(s"state_$i")
        val in_psum = Wire(accType)
        val acc_psum = Wire(accType)
        val sr_psum = ShiftRegister(in_psum, columns)
        val sr_last = ShiftRegister(io.in_last(i), columns, false.B, true.B)
        val sr_valid = ShiftRegister(io.in_valid(i), columns, false.B, true.B)

        val (cVal, cWrap) = Counter(state === Fsm.Output || state === Fsm.Init, 4)

        acc_psum := io.in_psum(i).asUInt + sr_psum.asUInt
        
        // in_psum := Mux( sr_last , 0.U, Mux(sr_valid, acc_psum, sr_psum))
        // in_psum := Mux( sr_last, acc_psum, )
        // in_psum := Mux( state === Fsm.Output, io.in_psum(i).asUInt, Mux(sr_valid, acc_psum, sr_psum) )
        in_psum := Mux( state === Fsm.Output || state === Fsm.Init , Mux(sr_valid, io.in_psum(i).asUInt, 0.U), Mux(sr_valid, acc_psum, sr_psum))
        // io.out_sum(i).bits := Mux( sr_last, acc_psum, 0.U )
        // io.out_sum(i).bits := Mux( state === Output, sr_psum, 0.U)
        io.out_sum(i).bits := sr_psum
        io.out_sum(i).valid := state === Fsm.Output
        // io.out_sum(i).valid := sr_last


        switch(state) {
            is(Fsm.Init) {
                when(cWrap) { state := Fsm.Acc }
            }
            is(Fsm.Acc) {
                when(sr_last) {
                    state := Fsm.Output
                }
            }
            is(Fsm.Output) {
                when(cWrap) { state := Fsm.Acc }
            }
        }



        (sr_psum, sr_last, sr_valid)
    }.unzip3
    
}

class StaticTransposer[T <: Data](
    eleWidth:           Int,
    rows:               Int,
    columns:            Int
) extends Module {

    require(rows == columns, "Transposer, rows should equal columns")


    val io = IO(new Bundle {
        val in = Input(Valid(Vec(rows, UInt(eleWidth.W))))
        val out = Output(Valid(Vec(columns, UInt(eleWidth.W))))
    })

    val PEs = Seq.fill(rows, columns) { 
        val r0 = Reg(UInt(eleWidth.W))
        val r1 = Reg(UInt(eleWidth.W))
        (r0, r1)
     }
     

    object Fsm extends ChiselEnum {
        val Idle, Transposing = Value
    }

    val cur_fsm = RegInit(Fsm.Idle)
    // val nxt_fsm = Wire(chiselTypeOf(cur_fsm))
    
    val (cntVal, cntWrap) = Counter(io.in.valid || (cur_fsm === Fsm.Transposing), columns)
    
    val valid             = ShiftRegister(io.in.valid || (cur_fsm === Fsm.Transposing), rows)
    io.out.valid := valid
    
    
    // val cond = Wire(Bool())
    val cond = RegInit(false.B)

    val data_in = Mux(io.in.valid, io.in.bits, VecInit(Seq.fill(rows){0.U(io.in.bits.head.getWidth.W)}))
    // VecInit(Seq.fill(rows){UInt.Lit(0, io.in.bits.head.getWidth)})

    when((cur_fsm === Fsm.Transposing) && cntWrap) {
        cond := ~cond
    }.elsewhen((cur_fsm === Fsm.Idle)) {
        cond := false.B
    }.otherwise {
        cond := cond
    }


    switch(cur_fsm) {
        is(Fsm.Idle) {
            when(io.in.valid) { cur_fsm := Fsm.Transposing }
            .otherwise { cur_fsm := Fsm.Idle }
        }
        is(Fsm.Transposing) {
            when(cntWrap) {
                when(io.in.valid) { cur_fsm := Fsm.Transposing }.otherwise { cur_fsm := Fsm.Idle }
            }.otherwise { cur_fsm := Fsm.Transposing }
        }
    }



    for(r <- 0 until rows) {
        for(c <- 0 until columns) {
            val (r0, r1): (UInt, UInt) = PEs(r)(c)
            val (lr0, lr1): (UInt, UInt) = if(c == 0) (data_in(r), data_in(r)) else PEs(r)(c-1)
            val (dr0, dr1): (UInt, UInt) = if(r == rows-1) (0.U, 0.U) else PEs(r+1)(c)

            if(r == rows-1) {
                when(cond) {
                    r0 := r0
                }.otherwise {
                    r0 := lr0
                }
                when(cond) {
                    r1 := lr1
                }.otherwise {
                    r1 := r1
                }
            } else {
                when(cond) {
                    r0 := dr0
                }.otherwise {
                    r0 := lr0
                }

                when(cond) {
                    r1 := lr1
                }.otherwise {
                    r1 := dr1
                }
            }

            if(r == 0) {
                when(cond) {
                    io.out.bits(c) := r0
                }.otherwise {
                    io.out.bits(c) := r1
                }
            }
        }
    }





}



class GemmCore[T <: Data](
    inputType:          T,
    outputType:         T,
    accType:            T,
    rows:               Int,
    columns:            Int,
    df:                 Dataflow.Value
)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {


    val io = IO(new Bundle {
        val in_a        = Input(Vec(rows, inputType))
        val in_b        = Input(Vec(columns, inputType))
        val in_d        = Input(Vec(columns, outputType))
        
        
        val in_control  = Input(Vec(columns, new PEControl(accType)))
        
        val in_valid    = Input(Vec(columns, Bool()))
        
        val in_last     = Input(Vec(columns, Bool()))
        
        val out_c       = Output(Valid(Vec(columns, accType)))

    })


    val meshWithDelay = Module(new MeshDelay(inputType, outputType, accType, rows, columns, df))

    val transposer    = Module(new StaticTransposer(inputType.getWidth, rows, columns))
    val accumulator   = Module(new Accumulator(inputType, outputType, accType, rows, columns, df))
    
    val wbAlignShiftRegister = Module(new CascadeDelayShiftRegister(columns, accType.getWidth, Seq.tabulate(columns){i => columns-i} ))



    transposer.io.in.valid := Cat(io.in_valid).orR
    // transposer.io.in.bits  := io.in_a
    transposer.io.in.bits.zipWithIndex.map { case (in, i) => {
        in := io.in_a(rows-1-i)
    }}


    // VecInit(Seq.fill(rows) { 0.U(transposer.io.out.bits.head.getWidth.W) })
    meshWithDelay.io.in_a := Mux( transposer.io.out.valid,  transposer.io.out.bits, VecInit(Seq.fill(rows) { 0.U(transposer.io.out.bits.head.getWidth.W) }))
    meshWithDelay.io.in_b := io.in_b
    meshWithDelay.io.in_d := io.in_d
    meshWithDelay.io.in_control := io.in_control
    meshWithDelay.io.in_valid := io.in_valid
    meshWithDelay.io.in_last := io.in_last


    accumulator.io.in_psum := meshWithDelay.io.out_psum
    accumulator.io.in_last := meshWithDelay.io.out_last
    accumulator.io.in_valid := meshWithDelay.io.out_valid
    accumulator.io.in_control := meshWithDelay.io.out_control



    wbAlignShiftRegister.io.in.zipWithIndex.map {case (in, i) => {
        in := accumulator.io.out_sum(i).bits
    }}

    // val valid = RegNext( accumulator.io.out_sum(0).valid )
    val valid = ShiftRegister(accumulator.io.out_sum(0).valid, 4, false.B, true.B)

    io.out_c.bits     := wbAlignShiftRegister.io.out
    io.out_c.valid    := valid
}

