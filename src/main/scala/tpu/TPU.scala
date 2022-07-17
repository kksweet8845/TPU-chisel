package tpu



import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{prefix, noPrefix}
import arithmetic._
import scala.math.pow
import tpu.util._



class SramIO(addrBits: Int, dataBits: Int) extends Bundle {
    val wr_en         = Input(Bool())
    val index         = Input(UInt(addrBits.W))
    val data_in       = Input(UInt(dataBits.W))
    val data_out      = Output(UInt(dataBits.W))
} 


class ControlRegIO extends Bundle {
    val K = UInt(8.W)
    val M = UInt(8.W)
    val N = UInt(8.W)
    val K_4 = UInt(8.W)
    val M_4 = UInt(8.W)
    val N_4 = UInt(8.W)
    val K_g = UInt(8.W)
    val M_g = UInt(8.W)
    val N_g = UInt(8.W)
}

object ControlRegIO {
    def apply[T <: Data](K: T, M: T, N: T, K_4: T, M_4: T, N_4: T, K_g: T, M_g: T, N_g: T): ControlRegIO = {
        val csr = Wire(new ControlRegIO)
        csr.K := K
        csr.M := M
        csr.N := N
        csr.K_4 := K_4
        csr.M_4 := M_4
        csr.N_4 := N_4
        csr.K_g := K_g
        csr.M_g := M_g
        csr.N_g := N_g
        csr
    }
}




class InputAddrGenUnit(
    rows:           Int, 
    columns:        Int, 
    inDataBits:     Int, 
    accDataBits:    Int, 
    addrBits:       Int,
    elePerGroup:    Int
) extends Module with RequireAsyncReset {

    val io = IO(new Bundle {
        val start       = Input(Bool())
        val csr         = Input(new ControlRegIO)
        val finish      = Output(Bool())

        val A           = Flipped(new SramIO(addrBits, inDataBits*elePerGroup))
        val B           = Flipped(new SramIO(addrBits, inDataBits*elePerGroup))
        val data_valid  = Output(Vec(columns, Bool()))
        val A_outbound  = Output(Bool())
        val B_outbound  = Output(Bool())
        val out_last    = Output(Bool())

        val wb_finished = Input(Bool())
    })

    

    val csr = io.csr
    val M   = csr.M
    val N   = csr.N
    val K   = csr.K
    val M_4 = csr.M_4
    val M_g = csr.M_g
    val N_4 = csr.N_4
    val N_g = csr.N_g
    val K_4 = csr.K_4
    val K_g = csr.K_g


    val A_base = withReset(reset.asAsyncReset) { RegInit(UInt(addrBits.W), 0.U) }
    val B_base = withReset(reset.asAsyncReset) { RegInit(UInt(addrBits.W), 0.U) }

    // val (aptr, aptr_nxt, aptr_prv) = Ptr(UInt(addrBits.W), 0.U)
    // val (bptr, bptr_nxt, bptr_prv) = Ptr(UInt(addrBits.W), 0.U)

    val aPtr                       = prefix("aPtr") { FifoPtr.makeUpdatePtr(pow(2, addrBits).toInt) }
    val bPtr                       = prefix("bPtr") { FifoPtr.makeUpdatePtr(pow(2, addrBits).toInt) }



    object SchedulerFsm extends ChiselEnum {
        val Idle, DetermineInitAddr, Fetching, DataGet, WaitWB, Finish = Value
    }


    val cur_fsm = withReset(reset.asAsyncReset) { RegInit(SchedulerFsm.Idle) }
    // val nxt_fsm = Wire(chiselTypeOf(cur_fsm))

    // val data_valid = withReset(reset.asAsyncReset) { WireInit(false.B) }
    val valid_bytelane = withReset(reset.asAsyncReset) { WireInit(VecInit(Seq.fill(columns) {false.B} )) }

    // io.data_valid := data_valid
    io.data_valid := valid_bytelane

    // cur_fsm := nxt_fsm

    io.A.wr_en := false.B
    io.A.index := aPtr.fp.ptr
    io.A.data_in := DontCare

    io.B.wr_en := false.B
    io.B.index := bPtr.fp.ptr
    io.B.data_in := DontCare

    val (tileVal, tileWrap) = Counter((cur_fsm === SchedulerFsm.Fetching), columns)
    val (kVal, kWrap, kOnLast) = DynCounter(tileWrap, chiselTypeOf(K), K_g, 1)
    val (mVal, mWrap, mOnLast) = DynCounter(kWrap, chiselTypeOf(M), M_g, 1)
    val (nVal, nWrap, nOnLast) = DynCounter(mWrap, chiselTypeOf(N), N_g, 1)


    val cntOff = Wire(chiselTypeOf(aPtr.fp.ptr))
    cntOff := (aPtr.fp.ptr(1,0) + K(1,0)) 

    io.A_outbound := kOnLast && K(1,0) =/= 0.U && ((aPtr.fp.ptr - A_base) >= K)
    io.B_outbound := (kOnLast && K(1,0) =/= 0.U && ((bPtr.fp.ptr - B_base) >= K )) || ((cur_fsm === SchedulerFsm.DetermineInitAddr) && K(7,2) =/= 0.U && bPtr.fp.ptr(1,0) > K(1,0))
    io.out_last   := kWrap



    when((cur_fsm === SchedulerFsm.Idle)) {
        A_base := 0.U
    }.elsewhen((cur_fsm === SchedulerFsm.Fetching)) {
        when(mWrap) { A_base := 0.U }
        .elsewhen(kWrap) { A_base := A_base + K }
    }

    when((cur_fsm === SchedulerFsm.Idle)) {
        B_base := 0.U
    }.elsewhen((cur_fsm === SchedulerFsm.Fetching)) {
        when(mWrap) { B_base := B_base + K }
    }

    //* A byte lane
    valid_bytelane.zipWithIndex.map{ case(valid, i) => {
        val mask = rows - 1 - i
        valid := !(M(1,0) =/= 0.U && mOnLast && mask.U >= M(1,0)) && (cur_fsm === SchedulerFsm.Fetching)
    }}



    when((cur_fsm === SchedulerFsm.Idle)) {
        kVal := 0.U
        nVal := 0.U
        mVal := 0.U
    }

    switch(cur_fsm) {
        is(SchedulerFsm.Idle) {
            aPtr.fpIn := aPtr.fp.copy(0.U, false.B)
            bPtr.fpIn := bPtr.fp.copy(0.U, false.B)
            aPtr.update := true.B
            bPtr.update := true.B
        }
        is(SchedulerFsm.DetermineInitAddr) {
            aPtr.fpIn := aPtr.fp.copy(A_base, false.B)
            bPtr.fpIn := bPtr.fp.copy(B_base + 3.U, false.B)
            aPtr.update := true.B
            bPtr.update := true.B
        }
        is(SchedulerFsm.Fetching) {
            when(mWrap) {
                aPtr.fpIn := aPtr.fp.copy(0.U, false.B)
                aPtr.update := true.B
            }.elsewhen(kWrap) {
                aPtr.fpIn := aPtr.fp.copy(A_base + K, false.B)
                aPtr.update := true.B
            }.elsewhen(tileWrap) {
                aPtr.update := true.B
            }.otherwise {
                aPtr.update := true.B
            }

            when(mWrap) {
                bPtr.fpIn := bPtr.fp.copy(B_base + K + 3.U, false.B)
                bPtr.update := true.B
            }.elsewhen(kWrap){
                bPtr.fpIn := bPtr.fp.copy(B_base + 3.U, false.B)
                bPtr.update := true.B
            }.elsewhen(tileWrap) {
                bPtr.fpIn := bPtr.fp.copy(bPtr.fp.ptr + 7.U, false.B)
                bPtr.update := true.B
            }.otherwise {
                bPtr.fpIn := bPtr.fp.minus1
                bPtr.update := true.B
            }
            
        }
    }
    
    io.finish := (cur_fsm === SchedulerFsm.WaitWB)


    switch(cur_fsm) {
        is(SchedulerFsm.Idle) {
            when(io.start) { cur_fsm := SchedulerFsm.DetermineInitAddr }
            .otherwise { cur_fsm := SchedulerFsm.Idle }
        }
        is(SchedulerFsm.DetermineInitAddr) {
            cur_fsm := SchedulerFsm.Fetching
        }
        is(SchedulerFsm.Fetching) {
            when(nWrap) { cur_fsm := SchedulerFsm.WaitWB }
            .otherwise { cur_fsm := SchedulerFsm.Fetching }
        }
        is(SchedulerFsm.WaitWB) {
            when(io.wb_finished) { cur_fsm := SchedulerFsm.Finish }
            .otherwise { cur_fsm := SchedulerFsm.WaitWB }
        }
        is(SchedulerFsm.Finish) {
            cur_fsm := SchedulerFsm.Idle
        }
    }
}


class InputDataController[T <: Data](
    rows:           Int,
    columns:        Int,
    inDataBits:     Int, 
    outDataBits:    Int, 
    addrBits:       Int,
    elePerGroup:    Int, 
    inputType:      T,
    outputType:     T,
    accType:        T,
    df:             Dataflow.Value) extends Module with RequireAsyncReset {


    require(rows == columns, "rows and columns should be equal")

    val bType = if(df == Dataflow.WS) outputType else accType
    val dType = if(df == Dataflow.WS) inputType else accType

    val io = IO(new Bundle {
        val start = Input(Bool())
        
        val A_data_in =  Input(UInt((inDataBits*elePerGroup).W))
        val A_data_mask = Input(Vec(rows, Bool()))
        val A_outbound = Input(Bool())
        val B_data_in = Input(UInt((inDataBits*elePerGroup).W))
        val B_data_mask = Input(Vec(columns, Bool()))
        val B_outbound = Input(Bool())
        val in_last     = Input(Bool())
        
        //* Output to PEs
        val out_a       = Output(Vec(rows, inputType))
        val out_b       = Output(Vec(columns, bType))
        val out_d       = Output(Vec(columns, dType))
        val out_control = Output(Vec(columns, new PEControl(accType)))
        val out_valid   = Output(Vec(columns, Bool())) //* valid when the weight or accumulate is true
        val out_last    = Output(Vec(columns, Bool()))
        val out_woff    = Output(UInt(log2Ceil(rows+1).W))

        val wb_finished = Input(Bool())
    })


    object CtrlFsm extends ChiselEnum {
        val Idle, Controlling, Finish = Value
    }

    val cur_fsm = RegInit(CtrlFsm.Idle)
    // val nxt_fsm = Wire(chiselTypeOf(cur_fsm))

    val A_data_valid = io.A_data_mask.asUInt.orR
    val B_data_valid = io.B_data_mask.asUInt.orR
    val A_data = Seq.tabulate(rows) { i => {
        val lo = i*inDataBits
        val hi = i*inDataBits + inDataBits - 1
        // val data = Mux(A_data_valid, 0.U(inDataBits.W), io.A_data_in.bits(hi, lo))
        val data = Mux(io.A_outbound || !A_data_valid, 0.U(inDataBits.W), io.A_data_in(hi, lo) )
        data
    }}
    val B_data = Seq.tabulate(columns) { i => {
        val lo = i*inDataBits
        val hi = i*inDataBits + inDataBits - 1

        // val data = Mux(B_data_valid, 0.U(inDataBits.W), io.B_data_in.bits(hi, lo))
        val data = Mux(io.B_outbound || !B_data_valid, 0.U(inDataBits.W), io.B_data_in(hi, lo) )
        data
    }}

    for(r <- 0 until rows) {
        io.out_a(r) := A_data(r)
    }

    val WEIGHTSTATION = Dataflow.WS.id.U(1.W)
    val OUTPUTSTATION = Dataflow.OS.id.U(1.W)

    //* control dataflow
    val dataflow = WireInit(WEIGHTSTATION)


    for(c <- 0 until columns) {
        when((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && dataflow === WEIGHTSTATION) ) {
            io.out_b(c) := 0.U
            io.out_d(c) := B_data(c)
        }.elsewhen((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B) && dataflow === OUTPUTSTATION) {
            io.out_b(c) := B_data(c)
            io.out_d(c) := 0.U //TODO preload with correct value
        }.otherwise{
            io.out_b(c) := DontCare
            io.out_d(c) := DontCare
        }
    }


    val (cntVal, cntWrap) = Counter(A_data_valid && B_data_valid, 4)
    val prog = RegInit(false.B)
    
    when((cur_fsm === CtrlFsm.Idle)) {
        prog := false.B
    }.elsewhen(cntWrap) {
        prog := ~prog
    }

    //TODO only support WS
    for(c <- 0 until columns) {
        io.out_control(c).dataflow     := dataflow
        io.out_control(c).propagate    := prog
        io.out_control(c).shift        := 0.U

        // io.out_valid(c)             := io.B_data_mask(c)
        io.out_valid(c)             := B_data_valid
        io.out_last(c)              := io.in_last
    }

    io.out_woff := io.A_data_mask.map(_.asUInt).reduce( _.asUInt +& _.asUInt )


    switch(cur_fsm) {
        is(CtrlFsm.Idle) {
            when(io.start) { cur_fsm := CtrlFsm.Controlling }
        }
        is(CtrlFsm.Controlling) {
            when(!A_data_valid) { cur_fsm := CtrlFsm.Finish }
        }
        is(CtrlFsm.Finish) {
            when(io.wb_finished) { cur_fsm := CtrlFsm.Idle }
        }
    }

}


class TPUCoreWrapper(
    rows:           Int, 
    columns:        Int, 
    inDataBits:     Int, 
    outDataBits:    Int, 
    accDataBits:    Int, 
    addrBits:       Int,
    elePerGroup:    Int,
    df:             Dataflow.Value
) extends Module with RequireAsyncReset {


    val io = IO(new Bundle {
        val start = Input(Bool())
        val csr   = Input(new ControlRegIO)
        val finished = Output(Bool())

        val A     = Flipped(new SramIO(addrBits, inDataBits*elePerGroup))
        val B     = Flipped(new SramIO(addrBits, inDataBits*elePerGroup))

        val C     = Flipped(new SramIO(addrBits, accDataBits*elePerGroup))

    })

    val inputType  = UInt(inDataBits.W)
    val outputType = UInt(outDataBits.W)
    val accType    = UInt(accDataBits.W)

    val inAGU      = Module( new InputAddrGenUnit(rows, columns, inDataBits, accDataBits, addrBits, elePerGroup) )
    val inCtrl     = Module( new InputDataController(rows, columns, inDataBits, outDataBits, addrBits, elePerGroup, inputType, outputType, accType, df))
    val gemm       = Module( new GemmCore(inputType, outputType, accType, rows, columns, df))
    val wbUnit     = Module( new WbController(inputType, outputType, accType, accDataBits, addrBits, elePerGroup, rows, columns, df))


    //* inAGU
    inAGU.io.start := io.start
    inAGU.io.csr := io.csr
    inAGU.io.A <> io.A
    inAGU.io.B <> io.B
    inAGU.io.wb_finished := wbUnit.io.finished

    //* Input Controller
    inCtrl.io.start := io.start
    inCtrl.io.A_data_in       := io.A.data_out
    inCtrl.io.A_data_mask     := inAGU.io.data_valid
    inCtrl.io.A_outbound      := inAGU.io.A_outbound
    inCtrl.io.B_data_in       := io.B.data_out
    inCtrl.io.B_outbound      := inAGU.io.B_outbound
    inCtrl.io.B_data_mask     := inAGU.io.data_valid
    inCtrl.io.in_last         := inAGU.io.out_last
    inCtrl.io.wb_finished     := wbUnit.io.finished


    //* gemm
    gemm.io.in_d := inCtrl.io.out_d
    gemm.io.in_a := inCtrl.io.out_a
    gemm.io.in_b := inCtrl.io.out_b

    gemm.io.in_control := inCtrl.io.out_control
    gemm.io.in_valid   := inCtrl.io.out_valid
    gemm.io.in_last    := inCtrl.io.out_last
    gemm.io.in_woff    := inCtrl.io.out_woff


    //* wbUnit
    wbUnit.io.start := io.start
    wbUnit.io.csr := io.csr
    wbUnit.io.in_c := gemm.io.out_c

    wbUnit.io.C <> io.C

    io.finished := wbUnit.io.finished

}




class TPU(
    rows:           Int, 
    columns:        Int, 
    inDataBits:     Int, 
    outDataBits:    Int, 
    accDataBits:    Int, 
    addrBits:       Int, 
    elePerGroup:    Int,
    df:             Dataflow.Value = Dataflow.WS
) extends RawModule {

    val clk             = IO(Input(Clock()))
    val rst_n           = IO(Input(AsyncReset()))
    val in_valid        = IO(Input(Bool()))
    val K               = IO(Input(UInt(8.W)))
    val M               = IO(Input(UInt(8.W)))
    val N               = IO(Input(UInt(8.W)))
    val busy            = IO(Output(Bool()))

    val A          = IO( Flipped(new SramIO(addrBits, inDataBits*elePerGroup)) )
    val B          = IO( Flipped(new SramIO(addrBits, inDataBits*elePerGroup)) )
    val C          = IO( Flipped(new SramIO(addrBits, accDataBits*elePerGroup)) )



    object TPUFsm extends ChiselEnum {
        val Idle, Fetching, Finish = Value
    }    


    // val rst = ~rst_n.asBool
    val rst = Wire(Bool())

    rst := ~rst_n.asBool

    val cur_fsm = withClockAndReset(clk, rst.asAsyncReset) { RegInit(TPUFsm.Idle) }
    // val nxt_fsm = Wire(chiselTypeOf(cur_fsm))
    val busyReg = withClockAndReset(clk, rst.asAsyncReset) { RegInit(chiselTypeOf(busy), false.B) }

    val k_reg = withClockAndReset(clk, rst.asAsyncReset) { RegInit(chiselTypeOf(K), 0.U) }
    val m_reg = withClockAndReset(clk, rst.asAsyncReset) { RegInit(chiselTypeOf(M), 0.U) }
    val n_reg = withClockAndReset(clk, rst.asAsyncReset) { RegInit(chiselTypeOf(N), 0.U) }
    
    val M_o = m_reg
    val N_o = n_reg
    val K_o = k_reg
    val M_4 = Wire(chiselTypeOf(M))
    val M_g = Wire(chiselTypeOf(M))
    val N_4 = Wire(chiselTypeOf(N))
    val N_g = Wire(chiselTypeOf(N))
    val K_4 = Wire(chiselTypeOf(K))
    val K_g = Wire(chiselTypeOf(K))

    M_4 := M_o >> 2;
    N_4 := N_o >> 2;
    K_4 := K_o >> 2;

    M_g := Mux((M_o(1,0) === 0.U), M_4, M_4+1.U)
    N_g := Mux(N_o(1,0) === 0.U, N_4, N_4+1.U)
    K_g := Mux(K_o(1,0) === 0.U, K_4, K_4 + 1.U)
    val csr = Wire(new ControlRegIO)
    csr.K := K_o
    csr.M := M_o
    csr.N := N_o
    csr.K_4 := K_4
    csr.M_4 := M_4
    csr.N_4 := N_4
    csr.K_g := K_g
    csr.M_g := M_g
    csr.N_g := N_g
    val start = (cur_fsm === TPUFsm.Fetching)

    withClockAndReset(clk, rst.asAsyncReset) {
        val core = Module(new TPUCoreWrapper(rows, columns, inDataBits, outDataBits, accDataBits, addrBits, elePerGroup, df))
        core.io.start := start
        core.io.csr   := csr
        core.io.A     <> A
        core.io.B     <> B
        core.io.C     <> C
        


        busy := busyReg

        switch(cur_fsm) {
            is(TPUFsm.Idle) {
                when(in_valid) {
                    busyReg := true.B
                    k_reg := K
                    m_reg := M
                    n_reg := N
                }
            }
            is(TPUFsm.Finish) {
                busyReg := false.B
            }
        }


        switch(cur_fsm) {
            is(TPUFsm.Idle) {
                when(in_valid) {
                    cur_fsm := TPUFsm.Fetching
                }.otherwise {
                    cur_fsm := TPUFsm.Idle
                }
            }
            is(TPUFsm.Fetching) {
                //* when scheduler finished the computation
                when(core.io.finished){
                    cur_fsm := TPUFsm.Finish
                }.otherwise {
                    cur_fsm := TPUFsm.Fetching
                }   
            }
            is(TPUFsm.Finish) {
                cur_fsm := TPUFsm.Idle
            }
        }
    }
}










