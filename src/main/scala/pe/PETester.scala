package pe

import chisel3._
import chisel3.util._
import config._

class PETesterTop(val position: (Int, Int) = (0, 0))(implicit val p:Parameters) extends Module {

  override def desiredName: String = "PE" + position.toString()

  val io = IO(new Bundle {
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg())
    val filter = Flipped(Decoupled(SInt(p(FilterW).W)))
    val img = Flipped(Decoupled(SInt(p(ImgW).W)))
    val pSumIn = Flipped(DecoupledIO(SInt(p(FilterW).W)))
    //    val oSumMEM = Decoupled(SInt(w.W))
    val oSumSRAM = Decoupled(SInt(p(OSumW).W))
    val stateOut = Output(UInt(4.W))
    val dataDone = Output(Bool())
    val totalFilterNum = Input(UInt(16.W))
    val totalSingleFilterNum = Input(UInt(16.W))
  })
  val pe = Module(new PE(position))
  val fIn = Queue(io.filter, 32)
  val iIn = Queue(io.img, 32)
  //  val oSumOut = Queue(pe.io.oSumMEM, 256)
  val oSumOut2 = Queue(pe.io.oSumSRAM, 2)
  //  core.dontTouch(pe.io.oSumMEM.ready)

  pe.io.totalSingleFilterNum := io.totalSingleFilterNum
  pe.io.totalFilterNum := io.totalFilterNum

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
