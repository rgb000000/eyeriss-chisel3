package pe

import chisel3._
import chisel3.util._

class PETesterTop(w:Int=16) extends Module{
  val io = IO(new Bundle{
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg())
    val fIn = Flipped(Decoupled(UInt(w.W)))
    val iIn = Flipped(Decoupled(UInt(w.W)))
    val oSum = Decoupled(UInt(w.W))
  })
  val pe = Module(new PE(256, 256, 32, 16))
  val fIn = Queue(io.fIn, 256)
  val iIn = Queue(io.iIn, 256)
  val oSumOut = Queue(pe.io.oSum, 256)
  oSumOut.ready := 1.U
  pe.io.filter <> fIn
  pe.io.img <> iIn
  pe.io.oSum <> io.oSum
  pe.io.regConfig := io.peconfig
  pe.io.pSumIn.valid := 0.U
  pe.io.pSumIn.bits := 1.U
  pe.io.stateSW := io.stateSW
}
