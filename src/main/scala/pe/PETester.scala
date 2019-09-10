package pe

import chisel3._
import chisel3.util._

class PETesterTop(position: (Int, Int) = (0, 0), w: Int = 8) extends Module {
  val io = IO(new Bundle {
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg())
    val filter = Flipped(Decoupled(SInt(w.W)))
    val img = Flipped(Decoupled(SInt(w.W)))
    val pSumIn = Flipped(DecoupledIO(SInt(w.W)))
    //    val oSumMEM = Decoupled(SInt(w.W))
    val oSumSRAM = Decoupled(SInt((16).W))
    val stateOut = Output(UInt(4.W))
    val dataDone = Output(Bool())
  })
  val pe = Module(new PE(256, 256, 256, w, position))
  val fIn = Queue(io.filter, 256)
  val iIn = Queue(io.img, 256)
  //  val oSumOut = Queue(pe.io.oSumMEM, 256)
  val oSumOut2 = Queue(pe.io.oSumSRAM, 256)
  //  core.dontTouch(pe.io.oSumMEM.ready)

  //  override def desiredName: String = position.toString()

  //  oSumOut.ready := 1.U
  //  oSumOut <> io.oSumMEM
  oSumOut2 <> io.oSumSRAM
  pe.io.filter <> fIn
  pe.io.img <> iIn
  //  pe.io.oSum <> io.oSum
  pe.io.regConfig := io.peconfig
  pe.io.pSumIn <> io.pSumIn
  pe.io.stateSW := io.stateSW

  io.stateOut := pe.io.stateOut
  io.dataDone := pe.io.dataDone
}
