package pe

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class test extends Module{
  val io = IO(new Bundle{
    val in = Input(UInt(16.W))
    val en = Input(UInt(1.W))
    val out = Output(UInt(16.W))
  })
  io.out := 0.U
  val a = Wire( DecoupledIO(UInt(3.W)))
  a.ready := 1.U
  a.valid := 1.U
  a.bits := io.in + 1.U
  when(io.en.asBool()){
    io.out := a.bits + a.ready + a.valid
  }

}
