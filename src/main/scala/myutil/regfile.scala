package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import pe.PEConfigReg
import axi._
import config._

class RegFile(val len: Int = 8, val aw: Int = 3, val dw: Int = 8)(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val we = Input(Bool())
    val addr = Input(UInt(aw.W))
    //    val waddr = Input(UInt(aw.W))
    val din = Input(UInt(8.W))
    val dout = Output(UInt(8.W))
    val peconfig = Output(new PEConfigReg)
    val go = Output(UInt(1.W))
    val loop = Output(UInt(8.W))
  })

  val regfile = RegInit(VecInit(Seq.fill(len)(0.U(dw.W))))
  // read
  io.dout := regfile(io.addr)
  when(io.we === 1.U) {
    // write
    regfile(io.addr) := io.din
  }
  io.peconfig.filterNum := regfile(0)
  io.peconfig.singleFilterLen := regfile(1)
  io.peconfig.imgNum := regfile(2)
  io.peconfig.singleImgLen := regfile(3)
  io.peconfig.nchannel := regfile(4)
  io.peconfig.relu := regfile(5)
  io.loop := regfile(6)
  io.go := regfile(7)

}

class AXIRegFile(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val host = new AXILiteClient(p(ShellKey).hostParams)

    val peconfig = Output(new PEConfigReg)
    val go = Output(UInt(1.W))
    val loop = Output(UInt(8.W))
  })

  val mp = p(ShellKey).memParams
  val hp = p(ShellKey).hostParams

  // regfile
  val regfile = RegInit(VecInit(Seq.fill(p(RegFileDepth))(0.U(p(RegFileW).W))))

  // Write control (AW, W, B)
  val waddr = RegInit("h_ffff".U(hp.addrBits.W)) // init with invalid address
  val wdata = io.host.w.bits.data
  val sWriteAddress :: sWriteData :: sWriteResponse :: Nil = Enum(3)
  val wstate = RegInit(sWriteAddress)

  // read control (AR, R)
  val sReadAddress :: sReadData :: Nil = Enum(2)
  val rstate = RegInit(sReadAddress)
  val rdata = RegInit(0.U(p(RegFileW).W))

  switch(wstate) {
    is(sWriteAddress) {
      when(io.host.aw.valid) {
        wstate := sWriteData
      }
    }
    is(sWriteData) {
      when(io.host.w.valid) {
        wstate := sWriteResponse
      }
    }
    is(sWriteResponse) {
      when(io.host.b.ready){
        wstate := sWriteAddress
      }
    }
  }

  when(io.host.aw.fire()){ waddr := io.host.aw.bits.addr(4,2)}
  io.host.aw.ready := wstate === sWriteAddress
  io.host.w.ready := wstate === sWriteData
  io.host.b.valid := wstate === sWriteResponse
  io.host.b.bits.resp := 0.U
  when(io.host.w.fire()){
    regfile(waddr) := wdata
  }

  switch(rstate){
    is(sReadAddress){
      when(io.host.ar.valid){
        rstate := sReadData
      }
    }
    is(sReadData){
      when(io.host.r.ready){
        rstate := sReadAddress
      }
    }
  }

  when(io.host.ar.fire()){ rdata := regfile(io.host.ar.bits.addr(4,2)) }
  io.host.ar.ready := rstate === sReadAddress
  io.host.r.valid := rstate === sReadData
  io.host.r.bits.data := rdata
  io.host.r.bits.resp := 0.U

  io.peconfig.filterNum := regfile(0)
  io.peconfig.singleFilterLen := regfile(1)
  io.peconfig.imgNum := regfile(2)
  io.peconfig.singleImgLen := regfile(3)
  io.peconfig.nchannel := regfile(4)
  io.peconfig.relu := regfile(5)
  io.loop := regfile(6)
  io.go := regfile(7)
}

class XilinxShell(implicit p: Parameters) extends RawModule {

  val hp = p(ShellKey).hostParams
  val mp = p(ShellKey).memParams

  val ap_clk = IO(Input(Clock()))
  val ap_rst_n = IO(Input(Bool()))
  val s_axi_control = IO(new XilinxAXILiteClient(hp))

  val shell = withClockAndReset(clock = ap_clk, reset = (~ap_rst_n).toBool) {
    Module(new AXIRegFile)
  }

  // host
  shell.io.host.aw.valid := s_axi_control.AWVALID
  s_axi_control.AWREADY := shell.io.host.aw.ready
  shell.io.host.aw.bits.addr := s_axi_control.AWADDR

  shell.io.host.w.valid := s_axi_control.WVALID
  s_axi_control.WREADY := shell.io.host.w.ready
  shell.io.host.w.bits.data := s_axi_control.WDATA
  shell.io.host.w.bits.strb := s_axi_control.WSTRB

  s_axi_control.BVALID := shell.io.host.b.valid
  shell.io.host.b.ready := s_axi_control.BREADY
  s_axi_control.BRESP := shell.io.host.b.bits.resp

  shell.io.host.ar.valid := s_axi_control.ARVALID
  s_axi_control.ARREADY := shell.io.host.ar.ready
  shell.io.host.ar.bits.addr := s_axi_control.ARADDR

  s_axi_control.RVALID := shell.io.host.r.valid
  shell.io.host.r.ready := s_axi_control.RREADY
  s_axi_control.RDATA := shell.io.host.r.bits.data
  s_axi_control.RRESP := shell.io.host.r.bits.resp
}

object GetVerilog extends App{
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Xilinx_test"), () => new XilinxShell)
}
