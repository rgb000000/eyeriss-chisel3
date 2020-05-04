package node

import axi._
import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import pe._
import myutil._
import config._
import ram._

@chiselName
class PEnArrayShellWithWB(implicit p: Parameters) extends Module {
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val io = IO(new Bundle {
    // Input
    val FilterBRAM = Flipped(new BRAMInterface(p(BRAMKey).dataW))
    val ImgBRAM = Flipped(new BRAMInterface(p(BRAMKey).dataW*n))
    // Output
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    // other Config
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)
    val go = Input(Bool())
    val done = Output(Bool())
  })

  val penarrayRST = Wire(Bool())
  dontTouch(penarrayRST)

  val freader = withReset(penarrayRST)(Module(new BRAMFilterReader))
  freader.io.inChannelGroup := io.peconfig.nchannel
  freader.io.r <> io.FilterBRAM
  freader.io.go := io.go
  freader.io.addr := io.peconfig.filterAddr
  freader.io.len := io.peconfig.singleFilterLen * p(Shape)._1.asUInt() * io.peconfig.filterNum * io.peconfig.nchannel
  val ireader = withReset(penarrayRST)(Module(new BRAMImgReaderWithPadding))
  ireader.io.inChannel := io.peconfig.nchannel
  ireader.io.topOrBottom := io.peconfig.topOrBottom
  ireader.io.replace := io.peconfig.replace
  ireader.io.r <> io.ImgBRAM
  ireader.io.go := (freader.io.done) & (!RegNext(freader.io.done))
  ireader.io.addr := io.peconfig.imgAddr
  ireader.io.len := io.peconfig.singleImgLen * io.peconfig.nchannel

  val penarray = withReset(penarrayRST)(Module(new PEnArray))
  penarray.io.Freader <> freader.io.dout
  penarray.io.Fid <> freader.io.fid

  penarray.io.Ireaders.foreach(_.valid := ireader.io.doutSplit.valid)
  (penarray.io.Ireaders, ireader.io.doutSplit.bits).zipped.foreach(_.bits := _)
  ireader.io.doutSplit.ready := penarray.io.Ireaders.last.ready

  val wb = Module(new NewWB)
  wb.io.forceOut := io.peconfig.forceOut
  wb.io.rowLength := io.peconfig.singleImgLen + 2.U - io.peconfig.singleFilterLen + 1.U // padding + 2
  (wb.io.dins, penarray.io.Write).zipped.foreach(_ <> _)
  io.done := wb.io.done
  penarrayRST := wb.io.done | reset.asBool()

  io.inputRowDataOut := wb.io.inputRowDataOut
  io.outValid := wb.io.outValid
  io.outAddr := wb.io.outAddr

  //other config
  val totalSingleFilterNum = WireInit(io.peconfig.singleFilterLen * io.peconfig.nchannel)
  val totalFilterNum = WireInit(totalSingleFilterNum * io.peconfig.filterNum)
  penarray.io.stateSW := io.stateSW
  penarray.io.peconfig := io.peconfig
  penarray.io.go := io.go
}

class PEnArrayShellWithWBTestTop(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    // Output
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))

    val done = Output(Bool())
    // other Config
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)
    val go = Input(Bool())
  })
  val filterBRAM = Module(new BRAM(p(BRAMKey).dataW, p(FilterMEMPath)))
  val imgBRAM = Module(new BRAM(p(BRAMKey).dataW*(p(Shape)._1 + p(Shape)._2 - 1), p(FeatureMEMPath)))
  val penarray = Module(new PEnArrayShellWithWB)

  io.inputRowDataOut := penarray.io.inputRowDataOut
  io.outValid := penarray.io.outValid
  io.outAddr := penarray.io.outAddr

  io.done := penarray.io.done
  penarray.io.FilterBRAM <> filterBRAM.io
  penarray.io.ImgBRAM <> imgBRAM.io
  penarray.io.stateSW := io.stateSW
  penarray.io.peconfig := io.peconfig
  penarray.io.go := io.go
}

object  GVPEnArrayShellWithWBTestTop extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_PEnArrayShellWithWBTestTop_verilog"),
    () => new PEnArrayShellWithWBTestTop())
}
