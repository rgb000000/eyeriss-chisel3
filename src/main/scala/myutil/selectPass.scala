package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._

class selectPass[T <: Data](data: T)(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(data))
    val en = Input(Bool())
    val out = DecoupledIO(data)
  })
  when(io.en){
    io.out <> io.in
  }.otherwise{
    io.out.valid := 0.U
    io.out.bits := 0.U.asTypeOf(data)
    io.in.ready := 0.U
  }
}
