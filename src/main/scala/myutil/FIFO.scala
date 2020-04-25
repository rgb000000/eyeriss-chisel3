package myutil

import chisel3._
import chisel3.experimental._
import chisel3.util._


/** A hardware module implementing a Queue
  * @param gen The type of data to queue
  * @param entries The max number of entries in the queue
  * @param pipe True if a single entry queue can run at full throughput (like a pipeline). The ''ready'' signals are
  * combinationally coupled.
  * @param flow True if the inputs can be consumed on the same cycle (the inputs "flow" through the queue immediately).
  * The ''valid'' signals are coupled.
  *
  * @example {{{
  * val q = Module(new Queue(UInt(), 16))
  * q.io.enq <> producer.io.out
  * consumer.io.in <> q.io.deq
  * }}}
  */
// copy from Quene
@chiselName
class FIFO[T <: Data](gen: T,
                      val entries: Int,
                      pipe: Boolean = false,
                      flow: Boolean = false)
//                     (implicit compileOptions: chisel3.core.CompileOptions)
  extends Module() {
  @deprecated("Module constructor with override _reset deprecated, use withReset", "chisel3")
  def this(gen: T, entries: Int, pipe: Boolean, flow: Boolean, override_reset: Option[Bool]) = {
    this(gen, entries, pipe, flow)
    this.override_reset = override_reset
  }
  @deprecated("Module constructor with override _reset deprecated, use withReset", "chisel3")
  def this(gen: T, entries: Int, pipe: Boolean, flow: Boolean, _reset: Bool) = {
    this(gen, entries, pipe, flow)
    this.override_reset = Some(_reset)
  }

  val genType = if (compileOptions.declaredTypeMustBeUnbound) {
    requireIsChiselType(gen)
    gen
  } else {
    if (DataMirror.internal.isSynthesizable(gen)) {
      chiselTypeOf(gen)
    } else {
      gen
    }
  }

  val io = IO(new QueueIO(genType, entries))

  private val ram = Mem(entries, genType)
  private val enq_ptr = Counter(entries)
  private val deq_ptr = Counter(entries)
  private val maybe_full = RegInit(false.B)

  private val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full
  private val do_enq = WireInit(io.enq.fire())
  private val do_deq = WireInit(io.deq.fire())

  when (do_enq) {
    ram(enq_ptr.value) := io.enq.bits
    enq_ptr.inc()
  }
  when (do_deq) {
    deq_ptr.inc()
  }
  when (do_enq =/= do_deq) {
    maybe_full := do_enq
  }

  io.deq.valid := !empty
  io.enq.ready := !full
  io.deq.bits := ram(deq_ptr.value)

  if (flow) {
    when (io.enq.valid) { io.deq.valid := true.B }
    when (empty) {
      io.deq.bits := io.enq.bits
      do_deq := false.B
      when (io.deq.ready) { do_enq := false.B }
    }
  }

  if (pipe) {
    when (io.deq.ready) { io.enq.ready := true.B }
  }

  private val ptr_diff = enq_ptr.value - deq_ptr.value
  if (isPow2(entries)) {
    io.count := Mux(maybe_full && ptr_match, entries.U, 0.U) | ptr_diff
  } else {
    io.count := Mux(ptr_match,
      Mux(maybe_full,
        entries.asUInt, 0.U),
      Mux(deq_ptr.value > enq_ptr.value,
        entries.asUInt + ptr_diff, ptr_diff))
  }
}

/** Factory for a generic hardware queue.
  *
  * @param enq input (enqueue) interface to the queue, also determines width of queue elements
  * @param entries depth (number of elements) of the queue
  *
  * @return output (dequeue) interface from the queue
  *
  * @example {{{
  * consumer.io.in <> Queue(producer.io.out, 16)
  * }}}
  */
object FIFO
{
  /** Create a queue and supply a DecoupledIO containing the product. */
  @chiselName
  def apply[T <: Data](
                        enq: ReadyValidIO[T],
                        entries: Int = 2,
                        pipe: Boolean = false,
                        flow: Boolean = false): DecoupledIO[T] = {
    if (entries == 0) {
      val deq = Wire(new DecoupledIO(enq.bits))
      deq.valid := enq.valid
      deq.bits := enq.bits
      enq.ready := deq.ready
      deq
    } else {
      require(entries > 0)
      val q = Module(new FIFO(chiselTypeOf(enq.bits), entries, pipe, flow))
      q.io.enq.valid := enq.valid // not using <> so that override is allowed
      q.io.enq.bits := enq.bits
      enq.ready := q.io.enq.ready
      TransitName(q.io.deq, q)
    }
  }

  /** Create a queue and supply a IrrevocableIO containing the product.
    * Casting from Decoupled is safe here because we know the Queue has
    * Irrevocable semantics; we didn't want to change the return type of
    * apply() for backwards compatibility reasons.
    */
  @chiselName
  def irrevocable[T <: Data](
                              enq: ReadyValidIO[T],
                              entries: Int = 2,
                              pipe: Boolean = false,
                              flow: Boolean = false): IrrevocableIO[T] = {
    require(entries > 0) // Zero-entry queues don't guarantee Irrevocability
    val deq = apply(enq, entries, pipe, flow)
    val irr = Wire(new IrrevocableIO(deq.bits))
    irr.bits := deq.bits
    irr.valid := deq.valid
    deq.ready := irr.ready
    irr
  }
}
