package tpu






import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.{prefix, noPrefix}
import arithmetic._


// case class WeightStationary extends StationaryMethod
// case class OutputStationary extends StationaryMethod
class PEControl[T <: Data](accType: T) extends Bundle {
    val dataflow = UInt(1.W)
    val propagate = UInt(1.W)
    val shift = UInt(log2Up(accType.getWidth).W)
}





class PE[T <: Data](inputType: T, outputType: T, accType: T, df: Dataflow.Value)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {

    import ar._

    val bType = if(df == Dataflow.WS) outputType else inputType


    val io = IO(new Bundle {
        val in_a = Input(inputType)
        val in_b = Input(bType)
        val in_d = Input(outputType)
        val out_a = Output(inputType)  
        val out_b = Output(bType)  
        val out_c = Output(outputType)

        val in_control = Input(new PEControl(accType))
        val out_control = Output(new PEControl(accType))

        val in_valid = Input(Bool())
        val out_valid = Output(Bool())
        val in_last = Input(Bool())
        val out_last = Output(Bool())
    })

    // implicit def mac(a: T, b: T, c: T): UInt = {
    //     a.asUInt * b.asUInt + c.asUInt
    // }

    val cType = if(df == Dataflow.WS) inputType else accType

    // val c1 = Reg(cType)
    // val c1 = RegInit(0.U(cType))
    val c1 =  prefix("c1") { RegInit(cType, cType.zero) }
    // val c2 = Reg(cType)
    // val c2 = RegInit(0.U(cType))
    val c2 = prefix("c2") { RegInit(cType, cType.zero) }

    val prog = io.in_control.propagate
    val shift = io.in_control.shift
    val dataflow = io.in_control.dataflow
    val valid = io.in_valid

    val last_p = RegEnable(prog, valid)
    val flipped = last_p =/= prog
    val shift_offset = Mux(flipped, shift, 0.U)


    val a = io.in_a
    val b = io.in_b
    val d = io.in_d

    io.out_a := a
    io.out_control.dataflow := dataflow
    io.out_control.shift := shift
    io.out_control.propagate := prog
    io.out_valid := valid
    io.out_last := io.in_last


    val OUTPUTSTATION = Dataflow.OS.id.U(1.W)
    val WEIGHTSTATION = Dataflow.WS.id.U(1.W)
    val PROG          = 1.U(1.W)


    // io.out_c := mac(io.in_a, io.in_b, io.in_c)

    when((df == Dataflow.WS).B || ((df == Dataflow.BOTH).B && dataflow === WEIGHTSTATION)) {
        when(prog === PROG) {
            c1 := d
            io.out_c := c1  //* Pass the weight to below PE
            // io.out_b := mac(a, c2.asTypeOf(inputType), b)
            io.out_b := b.mac(a, c2.asTypeOf(inputType))
        }.otherwise {
            c2 := d
            io.out_c := c2 //* Pass the weight to below PE
            // io.out_b := mac(a, c1.asTypeOf(inputType), b)
            io.out_b := b.mac(a, c1.asTypeOf(inputType))
        }
    }.elsewhen((df == Dataflow.OS).B || ((df == Dataflow.BOTH).B && dataflow === OUTPUTSTATION)) {
        when(prog === PROG) {
            c1 := d
            io.out_c := (c1 >> shift_offset.asTypeOf(inputType)) //* Prog the accumulate below the PE
            // c2 := mac(a, b, c2.asTypeOf(inputType))
            c2 := c2.mac(a, b)
            io.out_b := b
        }.otherwise {
            c2 := d
            io.out_c := (c2 >> shift_offset.asTypeOf(inputType)) //* Prog the accumulate below the PE
            // c1 := mac(a, b, c1.asTypeOf(inputType))
            c1 := c1.mac(a, b)
            io.out_b := b
        }
    }.otherwise {
        io.out_c := DontCare
        io.out_b := DontCare
    }

    when(!valid) {
        c1 := c1
        c2 := c2
    }


}






