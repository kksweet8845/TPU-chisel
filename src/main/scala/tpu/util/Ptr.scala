package tpu.util



import chisel3._
import chisel3.util._
// import chisel3.experimental.BundleLiterals._
// import chisel3.experimental.{prefix, noPrefix}
// import arithmetic._

// class Ptr[T <: Data](gen: T, init: T)(implicit ar: Arithmetic[T]) {

//     // val io = IO(new Bundle {
//     //     val q = Output(gen)
//     //     val d = Flipped(Valid(gen))
//     // })

//     val _ptr = RegInit(gen.asUInt, init)

//     // io.q := ptr    
//     // when(io.d.valid) {
//     //     ptr := io.d.bits
//     // }

//     def plus1 = _ptr := _ptr + 1.U
//     def minus1 = _ptr := _ptr - 1.U

//     def ptr = _ptr

//     def ptrNext: T = { 
//         val a = Wire(gen)
//         a := _ptr + 1.U
//         a
//     }
//     def ptrPrev: T = {
//         val a = Wire(gen)
//         a := _ptr - 1.U
//         a
//     }

//     def plus(off: T) = {
//         _ptr := _ptr + off.U
//     }

//     def minus(off: T) = {
//         _ptr := _ptr - off.U
//     }

//     def reset = {
//         _ptr := 0.U
//     }

// }

// class Ptr(entries: Int) extends Bundle {
//     val ptr = UInt(log2Ceil(entries).W)



//     def plus1: Ptr = {
//         if (isPow2(entries)) { (asUInt + 1.U).asTypeOf(chiselTypeOf(this)) }
//         else { Mux((ptr === (entries - 1).U), copy(ptr = 0.U, color = !color), copy(ptr = (ptr + 1.U))) }
//     }

//     def copy()

// }




// object Ptr {
//     def apply[T <: Data](gen: T, init: T = 0.U)(implicit ar: Arithmetic[T]): (Ptr[T], T, T) = {
//         val ptr = new Ptr(gen, init)
//         (ptr, ptr.ptrNext, ptr.ptrPrev)
//     }

//     def apply(entries: Int) = new Ptr(entries)

    


// }


/** Represent a FIFO pointer with the actual pointer and a color to distinguish "which trip around" (so you can tell empty from full).
  * This is useful mostly when you have a FIFO with more than just a single head pointer, and you need to be poking around in the
  * middle of the FIFO (otherwise just use a Queue).
  */
class FifoPtr(entries: Int) extends Bundle {
  val color = Bool() // MSB
  val ptr   = UInt(log2Ceil(entries).W)

  /** Create a copy of the pointer with potentially new values. */
  def copy(ptr: UInt = this.ptr, color: Bool = this.color): FifoPtr = {
    val result = Wire(chiselTypeOf(this))
    result.ptr   := ptr
    result.color := color
    result
  }

  /** Add 1, possibly wrapping.  When entries is a power of 2, this is just an adder. */
  def plus1: FifoPtr = {
    if (isPow2(entries)) { (asUInt + 1.U).asTypeOf(chiselTypeOf(this)) }
    else { Mux((ptr === (entries - 1).U), copy(ptr = 0.U, color = !color), copy(ptr = (ptr + 1.U))) }
  }

  /** Subtract 1, possibly wrapping.  When entries is a power of 2, this is just an adder. */
  def minus1: FifoPtr = {
    if (isPow2(entries)) { (asUInt - 1.U).asTypeOf(chiselTypeOf(this)) }
    else { Mux((ptr === 0.U), copy(ptr = (entries - 1).U, color = !color), copy(ptr = (ptr - 1.U))) }
  }

  /** Add 1, possibly wrapping, with a run-time limit.  In the case that the pointer has already passed
    * the limit, it continues until it hits the HW limit.  The user is in charge of figuring out when
    * it is safe to change the limit.
    */
  def plus1(limit: UInt): FifoPtr = {
    Mux((ptr === limit), copy(ptr = 0.U, color = !color), plus1)
  }

  /** Subtract 1, possibly wrapping, with a run-time limit.  In the case that limit is above the HW
    * limit, the HW limit is enforced.  The user is in charge of figuring out when it is safe to
    * change the limit.
    */
  def minus1(limit: UInt): FifoPtr = {
    Mux((ptr === 0.U), copy(ptr = (limit.min((entries - 1).U)), color = !color), copy(ptr = (ptr - 1.U)))
  }

  /** Equality check.  Pointers with different numbers of bits are always considered not equal. */
  def ===(that: FifoPtr): Bool = (this.asUInt.getWidth == that.asUInt.getWidth).B && (this.asUInt === that.asUInt)

  /** Inequality check.  Pointers with different numbers of bits are always considered not equal. */
  def =/=(that: FifoPtr): Bool = (this.asUInt.getWidth != that.asUInt.getWidth).B || (this.asUInt =/= that.asUInt)
}

/** Companion object for FifoPtr class, containing some utility methods. */
object FifoPtr {
  def apply(entries: Int) = new FifoPtr(entries)
  def apply(entries: Int, ptr: UInt, color: Bool) = WireDefault(new FifoPtr(entries), DontCare).copy(ptr, color)

  /** Return true if the given head/tail indicate empty. */
  def empty(headPtr: FifoPtr, tailPtr: FifoPtr): Bool = (headPtr === tailPtr)

  /** Return true if the given head/tail indicate full. */
  def full(headPtr: FifoPtr, tailPtr: FifoPtr): Bool = ((headPtr.ptr === tailPtr.ptr) && (headPtr.color =/= tailPtr.color))

  /** Create a set of FifoPtr's in a pattern that is useful for the common instantiation pattern. */
  case class UpdatePtr(
    val fp:     FifoPtr, // current cycle
    val fpNxt:  FifoPtr, // next cycle
    val fpP1:   FifoPtr, // curent cycle plus 1
    val update: Bool,    // update enable
    val fpIn:   FifoPtr, // update value
  )
  def makeUpdatePtr(entries: Int): UpdatePtr = {
    val update = WireDefault(false.B)
    val fpIn   = Wire(new FifoPtr(entries))
    val fp     = RegEnable(fpIn, 0.U.asTypeOf(new FifoPtr(entries)), update)
    fpIn := fp.plus1 // Default is to update to next entry; you can override with chisel last-connect semantics.
    val fpNxt = Mux(update, fpIn, fp)
    UpdatePtr(fp, fpNxt, fp.plus1, update, fpIn)
  }
}




