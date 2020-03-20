package pe

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import myutil._
import config._

class PEn(val position: (Int, Int) = (0, 0))(implicit p: Parameters) extends Module {
  val memW = p(BRAMKey).dataW
  val n = memW / p(FilterW)
  val io = IO(new Bundle {
    // [Input] data waiting for calculate
    val filter = Flipped(DecoupledIO(UInt(memW.W)))
    val img = Flipped(DecoupledIO(UInt(memW.W)))
    val oSumSRAM = DecoupledIO(SInt(p(OSumW).W))
    val iSum = Flipped(DecoupledIO(SInt(p(OSumW).W)))
    // [Input] config info
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg())
    val totalFilterNum = Input(UInt(16.W))
    val totalSingleFilterNum = Input(UInt(16.W))
    // [Output] some done signal
    val stateOut = Output(UInt(4.W))
    val dataDone = Output(Bool())
  })

  val pen = List.fill(n)(Module(new PETesterTop))

  val fSplit = Wire(Vec(n, SInt(p(FilterW).W)))
  val iSplit = Wire(Vec(n, SInt(p(FilterW).W)))
  for (i <- 0 until n) {
    fSplit(i) := io.filter.bits((i + 1) * p(FilterW) - 1, i * p(FilterW)).asSInt()
    iSplit(i) := io.img.bits((i + 1) * p(ImgW) - 1, i * p(ImgW)).asSInt()
  }

  for (i <- 0 until n) {
    // filter
    pen(i).io.filter.valid := io.filter.valid
    pen(i).io.filter.bits := fSplit(i)
    io.filter.ready := pen.head.io.filter.ready
    // img
    pen(i).io.img.valid := io.img.valid
    pen(i).io.img.bits := iSplit(i)
    io.img.ready := pen.head.io.img.ready
    // state and other
    pen(i).io.stateSW := io.stateSW
    pen(i).io.peconfig := io.peconfig
    pen(i).io.totalFilterNum := io.totalFilterNum
    pen(i).io.totalSingleFilterNum := io.totalSingleFilterNum
  }
  // oSum
  val addT = addTree(pen.map(_.io.oSumSRAM.bits) :+ io.iSum.bits)
  io.oSumSRAM.valid := pen.head.io.oSumSRAM.valid
  io.oSumSRAM.bits := addT
  pen.foreach(_.io.oSumSRAM.ready := io.oSumSRAM.ready)
  io.iSum.ready := pen.head.io.oSumSRAM.valid & io.oSumSRAM.ready

  io.stateOut := pen.head.io.stateOut
  io.dataDone := pen.head.io.dataDone
}

object GetVerilogPEn extends App {
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_PEn_vcd"), () => new PEn)
}
