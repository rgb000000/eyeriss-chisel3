package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._

class BRAMInterface(val memW: Int)(implicit val p: Parameters) extends Bundle {
  val addr = Input(UInt(p(BRAMKey).addrW.W))
  val din = Input(UInt(memW.W))
  val dout = Output(UInt(memW.W))
  val we = Input(Bool())
}
