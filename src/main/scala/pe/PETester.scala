package pe

import chisel3._
import chisel3.util._

class PETesterTop(w:Int=16) extends Module{
  val io = IO(new Bundle{
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg())
    val filter = Flipped(Decoupled(SInt(w.W)))
    val img = Flipped(Decoupled(SInt(w.W)))
    val pSumIn = Flipped(DecoupledIO(SInt(w.W)))
    val oSum = Decoupled(SInt(w.W))
  })
  val pe = Module(new PE(256, 256, 256, 16))
  val fIn = Queue(io.filter, 256)
  val iIn = Queue(io.img, 256)
  val oSumOut = Queue(pe.io.oSum, 256)
  oSumOut.ready := 1.U
  pe.io.filter <> fIn
  pe.io.img <> iIn
  pe.io.oSum <> io.oSum
  pe.io.regConfig := io.peconfig
  pe.io.pSumIn <> io.pSumIn
  pe.io.stateSW := io.stateSW
}
