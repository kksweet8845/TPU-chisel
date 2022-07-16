package tpu




import chisel3._
import chisel3.util._
import arithmetic._





class Mesh[T <: Data](
    inputType: T,
    outputType: T,
    accType: T,
    rows: Int,
    columns: Int,
    df: Dataflow.Value
)(implicit ar: Arithmetic[T]) extends Module with RequireAsyncReset {


    val io = IO(new Bundle {
        val in_a = Input(Vec(rows, inputType))
        val in_b = Input(Vec(columns, inputType))
        val in_d = Input(Vec(columns, outputType))
        val out_a = Output(Vec(rows, inputType))
        val out_b = Output(Vec(columns, inputType))
        val out_c = Output(Vec(columns, outputType))

        val in_control = Input(Vec(columns, new PEControl(accType)))
        val out_control = Output(Vec(columns, new PEControl(accType)))

        val in_valid = Input(Vec(columns, Bool()))
        val out_valid = Output(Vec(columns, Bool()))

        val in_last = Input(Vec(columns, Bool()))
        val out_last = Output(Vec(columns, Bool()))
    })




    val PEs = Seq.fill(rows, columns){ Module(new PE(inputType, outputType, accType, df)) }
    val PEsT = PEs.transpose

    val in_a = io.in_a
    val in_b = io.in_b
    val in_d = io.in_d  

    val out_a = io.out_a
    val out_b = io.out_b
    val out_c = io.out_c
    val in_control = io.in_control
    val out_control = io.out_control
    val in_valid = io.in_valid
    val out_valid = io.out_valid
    val in_last = io.in_last
    val out_last = io.out_last


    val interA = Seq.fill(rows, columns){ Reg(inputType) }
    val interB = Seq.fill(rows, columns){ Reg(inputType) }
    val interBT = interB.transpose
    val interC = Seq.fill(rows, columns){ Reg(outputType) }
    val interCT = interC.transpose
    val interCtrl = Seq.fill(rows, columns){ Reg(new PEControl(accType)) }
    val interCtrlT = interCtrl.transpose
    val interValid = Seq.fill(rows, columns){ Reg(Bool()) }
    val interValidT = interValid.transpose
    val interLast = Seq.fill(rows, columns){ Reg(Bool()) }
    val interLastT = interLast.transpose



    //* connect the in_a
    for(r <- 0 until rows) {
        PEs(r).zip(interA(r)).foldLeft(in_a(r)) {
            case (in_a, (pe, oR)) => {
                pe.io.in_a := in_a
                oR := pe.io.out_a
                oR
            }
        }
        // PEs(r).foldLeft(in_a(r)){
        //     (in_a, pe) => {
        //         pe.io.in_a := in_a
                
        //         val oR = RegNext(pe.io.out_a)
        //         interA = interA :+ oR
        //         oR
        //     }
        // }
    }

    //* connect in_b
    for(c <- 0 until columns) {

        PEsT(c).zip(interBT(c)).foldLeft(in_b(c)) {
            case (in_b, (pe, oR)) => {
                pe.io.in_b := in_b
                oR := pe.io.out_b
                oR
            }
        }

        // PEsT(c).foldLeft(in_b(c)) {
        //     (in_b, pe) => {
        //         pe.io.in_b := in_b
        //         val oR = RegNext(pe.io.out_b)
        //         interB = interB :+ oR
        //         oR
        //     }
        // }
    }


    //* connect in_d
    for(c <- 0 until columns) {
        PEsT(c).zip(interCT(c)).foldLeft(in_d(c)) {
            case (in_d, (pe, oR)) => {
                pe.io.in_d := in_d
                oR := pe.io.out_c
                oR
            }
        }

        // PEsT(c).foldLeft(in_d(c)) {
        //     (in_d, pe) => {
        //         pe.io.in_d := in_d
        //         val oR = RegNext(pe.io.out_c)
        //         interC = interC :+ oR
        //         oR
        //     }
        // }
    }

    //* connect in_control
    for(c <- 0 until columns) {
        PEsT(c).zip(interCtrlT(c)).foldLeft(in_control(c)) {
            case (in_control, (pe, oR)) => {
                pe.io.in_control := in_control
                oR := pe.io.out_control
                oR
            }
        }

        // PEsT(c).foldLeft(in_control(c)) {
        //     (in_control, pe) => {
        //         pe.io.in_control := in_control
        //         val oR = RegNext(pe.io.out_control)
        //         interCtrl = interCtrl :+ oR
        //         oR
        //     }
        // }
    }

    //* connect in_valid
    for(c <- 0 until columns) {
        PEsT(c).zip(interValidT(c)).foldLeft(in_valid(c)) {
            case (in_valid, (pe, oR)) => {
                pe.io.in_valid := in_valid
                oR := pe.io.out_valid
                oR
            }
        }

        // PEsT(c).foldLeft(in_valid(c)) {
        //     (in_valid, pe) => {
        //         pe.io.in_valid := in_valid
        //         val oR = RegNext(pe.io.out_valid)
        //         interValid = interValid :+ oR
        //         oR
        //     }
        // }
    }

    //* connect in_last
    for(c <- 0 until columns) {
        PEsT(c).zip(interLastT(c)).foldLeft(in_last(c)) {
            case (in_last, (pe, oR)) => {
                pe.io.in_last := in_last
                oR := pe.io.out_last
                oR
            }
        }
        // PEsT(c).foldLeft(in_last(c)) {
        //     (in_last, pe) => {
        //         pe.io.in_last := in_last
        //         val oR = RegNext(pe.io.out_last)
        //         interLast = interLast :+ oR
        //     }
        // }
    }

    
    //* connect last data

    for(r <- 0 until rows) {
        out_a(r) := interA(r)(columns-1)
    }

    for(c <- 0 until columns) {
        out_c(c) := interCT(c)(rows-1)
        out_b(c) := interBT(c)(rows-1)
        out_control(c) := interCtrlT(c)(rows-1)
        out_valid(c) := interValidT(c)(rows-1)
        out_last(c) := interLastT(c)(rows-1)
    }


}









