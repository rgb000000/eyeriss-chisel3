package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._

class BRAMInterface(implicit val p: Parameters) extends Bundle{
  val addr = Input(UInt(p(BRAMKey).addrW.W))
  val din = Input(UInt(p(BRAMKey).dataW.W))
  val dout = Output(UInt(p(BRAMKey).dataW.W))
  val we = Input(Bool())
}
