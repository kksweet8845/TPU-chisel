package tpu




import chisel3._
import chisel3.util._
import arithmetic._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{prefix, noPrefix}




class CascadeDelayShiftRegister[T <: Data](
    len:                Int,
    elemWidth:               Int
)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {



    val io = IO(new Bundle {
        val in = Input(Vec(len, UInt(elemWidth.W)))
        val out = Output(Vec(len, UInt(elemWidth.W)))
    })


    val cascade = io.in.zipWithIndex.map { case(ele, i) => {
        val sr = ShiftRegister(ele, i+1)
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }}

    cascade.zipWithIndex.map { case(o, i) => {
        io.out(i) := o
    }}
}



class MeshDelay[T <: Data](
    inputType:      T, 
    outputType:     T, 
    accType:        T, 
    rows:           Int,
    columns:        Int,
    df:             Dataflow.Value
)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {


    val io = IO(new Bundle {
        val in_a        = Input(Vec(rows, inputType))
        val in_b        = Input(Vec(columns, inputType))
        val in_d        = Input(Vec(columns, outputType))
        // val out_a       = Output(Vec(rows, inputType))
        // val out_b       = Output(Vec(columns, inputType))
        // val out_c       = Output(Vec(columns, outputType))
        
        val in_control  = Input(Vec(columns, new PEControl(accType)))
        val out_control = Output(Vec(columns, new PEControl(accType)))
        val in_valid    = Input(Vec(columns, Bool()))
        val out_valid   = Output(Vec(columns, Bool()))
        val in_last     = Input(Vec(columns, Bool()))
        val out_last    = Output(Vec(columns, Bool()))

        val out_psum        = Output(Vec(columns, outputType))



    })


    val OUTPUTSTATION = Dataflow.OS.id.U(1.W)
    val WEIGHTSTATION = Dataflow.WS.id.U(1.W)

    val mesh = Module(new Mesh(inputType, outputType, accType, rows, columns, df))

    val dataflow       = mesh.io.out_control.head.dataflow
    val isWS           = (df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && dataflow === WEIGHTSTATION)
    val isOS           = (df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && dataflow === OUTPUTSTATION)

    //* shift Registers

    val cascadeDelayA = Seq.tabulate(rows) { i => 
        // val ii = i + 1
        val sr = ShiftRegister(io.in_a(i), rows-i)
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }


    val cascadeDelayB = Seq.tabulate(columns) { i => 
        // val ii = i
        val sr = ShiftRegister(io.in_b(i), columns - i) 
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }


    val cascadeDelayC = Seq.tabulate(columns) { i => 
        // val ii = i + 1
        val sr = ShiftRegister(io.in_d(i), columns-i)
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }

    val cascadeDelayCtrl = Seq.tabulate(columns) { i => 
        // val ii = i + 1
        val sr = ShiftRegister(io.in_control(i), columns-i)
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }


    val cascadeDelayValid = Seq.tabulate(columns) { i => 
        // val ii = i + 1
        val sr = ShiftRegister(io.in_valid(i), columns-i)
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }

    val cascadeDelayLast = Seq.tabulate(columns) { i => 
        // val ii = i + 1
        val sr = ShiftRegister(io.in_last(i), columns - i)
        val out = Wire(chiselTypeOf(sr))
        out := sr
        out
    }




    for( r <- 0 until rows) {
        mesh.io.in_a(r) := cascadeDelayA(rows-1-r)
        // io.out_a(r)     := mesh.io.out_a(r)
    }

    for( c <- 0 until columns) {
        mesh.io.in_b(c)         := cascadeDelayB(columns-1-c)
        mesh.io.in_d(c)         := cascadeDelayC(columns-1-c)
        mesh.io.in_control(c)   := cascadeDelayCtrl(columns-1-c)
        mesh.io.in_valid(c)     := cascadeDelayValid(columns-1-c)
        mesh.io.in_last(c)      := cascadeDelayLast(columns-1-c)

        io.out_psum(c)          := Mux(isWS, mesh.io.out_b(c), mesh.io.out_c(c))
        io.out_control(c)   := mesh.io.out_control(c)
        io.out_valid(c)     := mesh.io.out_valid(c)
        io.out_last(c)      := mesh.io.out_last(c)
    }

}


