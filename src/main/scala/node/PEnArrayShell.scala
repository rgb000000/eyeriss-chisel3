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
    val last = Output(Bool())
    //maxPooling
    val pooloutValid = Output(Bool())
    val pooloutAddr = Output(UInt(p(URAMKey).addrW.W))
    val poolinputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
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
  io.last := wb.io.last
  wb.io.forceOut := io.peconfig.forceOut
  wb.io.rowLength := io.peconfig.singleImgLen + 2.U - io.peconfig.singleFilterLen + 1.U // padding + 2
  (wb.io.dins, penarray.io.Write).zipped.foreach(_ <> _)
  io.done := wb.io.done
  penarrayRST := wb.io.done | reset.asBool()
  val maxPool = Module(new MaxPoolingOut)
  maxPool.io.forceOut := io.peconfig.forceOut
  maxPool.io.inRowDataOut := wb.io.inputRowDataOut
  maxPool.io.inValid := wb.io.outValid
  maxPool.io.inAddr := wb.io.outAddr
  maxPool.io.length := wb.io.rowLength
  io.poolinputRowDataOut := maxPool.io.RowDataOut
  io.pooloutValid := maxPool.io.outValid
  io.pooloutAddr := maxPool.io.outAddr

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
    val last = Output(Bool())
    //maxPooling
    val pooloutValid = Output(Bool())
    val pooloutAddr = Output(UInt(p(URAMKey).addrW.W))
    val poolinputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))

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
  io.last := penarray.io.last
  io.poolinputRowDataOut := penarray.io.poolinputRowDataOut
  io.pooloutValid := penarray.io.pooloutValid
  io.pooloutAddr := penarray.io.pooloutAddr

  io.done := penarray.io.done
  penarray.io.FilterBRAM <> filterBRAM.io
  penarray.io.ImgBRAM <> imgBRAM.io
  penarray.io.stateSW := io.stateSW
  penarray.io.peconfig := io.peconfig
  penarray.io.go := io.go
}

class PEnArrayWBXilinxShell(implicit p: Parameters) extends RawModule{
  val hp = p(ShellKey).hostParams
  val mp = p(ShellKey).memParams

  val ap_clk = IO(Input(Clock()))
  val ap_rst_n = IO(Input(Bool()))
  val s_axi_control = IO(new XilinxAXILiteClient(hp))
  val axi_reg = Module(new XilinxShell)
  axi_reg.ap_clk := ap_clk
  axi_reg.ap_rst_n := ap_rst_n
  axi_reg.s_axi_control <> s_axi_control

  val sys_clk = IO(Input(Clock()))
  val sys_rst_n = IO(Input(Bool()))
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val pearrayio = IO(new Bundle{
    // Output
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val last = Output(Bool())
    //maxPooling
    val pooloutValid = Output(Bool())
    val pooloutAddr = Output(UInt(p(URAMKey).addrW.W))
    val poolinputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))

    val done = Output(Bool())
    // other Config
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)
    val go = Input(Bool())
  })
  val penarray = withClockAndReset(clock = sys_clk, reset = sys_rst_n){
    Module(new PEnArrayShellWithWBTestTop)
  }
  val ctrl = withClockAndReset(clock = sys_clk, reset = sys_rst_n){
    Module(new PEnController)
  }
  ctrl.io.go := axi_reg.out.go
  penarray.io.stateSW <> ctrl.io.stateSW
  penarray.io.peconfig <> axi_reg.out.peconfig
  penarray.io.go <> axi_reg.out.go
  penarray.io.done <> pearrayio.done

  pearrayio.outValid := penarray.io.outValid
  pearrayio.outAddr := penarray.io.outAddr
  pearrayio.inputRowDataOut := penarray.io.inputRowDataOut
  pearrayio.last := penarray.io.last
  pearrayio.pooloutAddr := penarray.io.pooloutAddr
  pearrayio.pooloutValid := penarray.io.pooloutValid
  pearrayio.poolinputRowDataOut := penarray.io.poolinputRowDataOut
}

object  GVPEnArrayShellWithWBTestTop extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_PEnArrayShellWithWBTestTop_verilog"),
    () => new PEnArrayShellWithWBTestTop())
}

object  GVPEnArrayXilinxShellWithWBTestTop extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_PEnArrayXilinxShellWithWBTestTop_verilog"),
    () => new PEnArrayWBXilinxShell)
}
