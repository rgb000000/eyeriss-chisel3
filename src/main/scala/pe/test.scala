package pe

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class Test extends Module{
  val io = IO(new Bundle{
    val in = Input(SInt(15.W))
    val out = Output(Bool())
  })

  io.out := io.in === (-1).S

}
