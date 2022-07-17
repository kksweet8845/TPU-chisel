package tpu




import chisel3._
import chisel3.util._
import arithmetic._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{prefix, noPrefix}
import tpu.util._
import scala.math.pow




class WbController[T <: Data](
    inputType:      T,
    outputType:     T,
    accType:        T,
    accDataBits:    Int,
    addrBits:       Int,
    elePerGroup:    Int,
    rows:           Int,
    columns:        Int,
    df:             Dataflow.Value
)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {

    val io = IO(new Bundle {
        val start       = Input(Bool())
        val csr         = Input(new ControlRegIO)
        val in_c        = Flipped(Valid(Vec(columns, accType)))
        val C           = Flipped(new SramIO(addrBits, accDataBits*elePerGroup))
        val finished    = Output(Bool())
    })


    object Fsm extends ChiselEnum {
        val Idle, InitW, Running = Value
    }

    val cur_fsm = withReset(reset.asAsyncReset) { RegInit(Fsm.Idle) }
    val freg            = RegInit(false.B)
    val cPtr            = FifoPtr.makeUpdatePtr(pow(2, addrBits).toInt)
    val r_in_c          = Wire(Vec(columns, accType))


    r_in_c.zipWithIndex.map{ case(in, i) => {
        in := io.in_c.bits(columns-1-i)
    }}


    when((cur_fsm === Fsm.Idle)) {
        // cptr.reset
        cPtr.fpIn := cPtr.fp.copy(0.U, false.B)
        cPtr.update := true.B
    }.elsewhen((cur_fsm === Fsm.Running)) {
        when(io.in_c.valid) { cPtr.update := true.B }
        .otherwise{ cPtr.update := false.B }
    }.otherwise {
        cPtr.update := false.B
    }


    freg := io.C.wr_en && cPtr.fp.ptr === (io.csr.N_g * io.csr.M - 1.U)
    io.finished := freg
    // io.finished := (cur_fsm === Fsm.Running) && cPtr.fp.ptr === (io.csr.N_g * io.csr.M - 1.U) && io.C.wr_en

    io.C.wr_en := (cur_fsm === Fsm.Running) && (io.in_c.valid)
    io.C.index := cPtr.fp.ptr
    // io.C.data_in := io.in_c.bits.asUInt
    io.C.data_in := r_in_c.asUInt



    // cur_fsm := nxt_fsm
    switch(cur_fsm) {
        is(Fsm.Idle) {
            when(io.start) { cur_fsm := Fsm.Running }
            .otherwise { cur_fsm := Fsm.Idle }
        }
        is(Fsm.Running) {
            when(io.finished) { cur_fsm := Fsm.Idle }
            .otherwise { cur_fsm := Fsm.Running }
        }
    }

}














