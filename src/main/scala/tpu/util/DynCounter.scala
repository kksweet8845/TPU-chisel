
package tpu.util


import chisel3._
import chisel3.util._
import arithmetic._



class DynCounter(gen: UInt, until: UInt, step: Int) {


    val value = RegInit(gen, 0.U)

    def inc() : Bool = {
        // val wrap = value === (until - 1.U)

        val wrap = Wire(Bool())
        when(until === 0.U) {
            wrap := true.B
        }.otherwise {
            wrap := value === (until - 1.U)
        }


        if(step > 0) {
            value := value + step.U
        } else {
            value := value - step.U
        }

        when(wrap) {
            value := 0.U
        }

        wrap
    }

}


object DynCounter {
    def apply(en: Bool, gen: UInt, until: UInt, step: Int): (UInt, Bool) = {
        val c = new DynCounter(gen, until, step)
        val wrap = WireInit(false.B)
        when(en) { wrap := c.inc() }
        (c.value.asUInt, wrap)
    }
}





