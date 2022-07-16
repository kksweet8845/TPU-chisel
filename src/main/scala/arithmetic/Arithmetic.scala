package arithmetic


import chisel3._
import chisel3.util._
import scala.language.implicitConversions
import chisel3.experimental.ChiselEnum

abstract class Arithmetic[A <: Data] {
    implicit def cast(self: A) : ArithmeticOps[A]
}



abstract class ArithmeticOps[A <: Data](self: A) {
    def mac(a: A, b: A): A
    def >>(shift: A) : A
    // def ##(that: A) : A
    // def %(that: A) : A
    def *(that: A) : A
    def +(that: A) : A
    // def +%(that: A) : A
    // def +&(that: A) : A
    def -(that: A) : A
    def zero: A
    def identity: A
    def getWidth: Int = self.getWidth
    // def -%(that: A) : A
    // def -&(that: A) : A
}





object Arithmetic {
    implicit object UIntArithmetic extends Arithmetic[UInt] {
        override implicit def cast(self: UInt) = new ArithmeticOps(self) {
            override def mac(a: UInt, b: UInt): UInt = a * b + self
            override def *(that: UInt): UInt = self * that
            override def +(that: UInt): UInt = self + that
            override def -(that: UInt): UInt = self - that
            override def >>(d: UInt): UInt = {
                //* roude-to-nearst-up (add + 0.5 LSB)
                val r = self(d-1.U)
                val round = (self.asUInt() >> d) + r
                round
            }
            override def zero: UInt = 0.U
            override def identity: UInt = 1.U
        }
    }
}






















