package top

import chisel3._
import chisel3.core.withReset
import chisel3.internal.naming.chiselName
import chisel3.util._
import node._
import myutil._
import pe._

class regfileInterface(val aw:Int = 3, val dw:Int = 8) extends Bundle{
  val we = Input(Bool())
  val raddr = Input(UInt(aw.W))
  val waddr = Input(UInt(aw.W))
  val din = Input(UInt(dw.W))
  val dout = Output(UInt(dw.W))
}

class Top(val faddr:Int = 0x0000, val iaddr:Int = 0x0480, val aw:Int = 3, val dw:Int = 8) extends Module{
  val io = IO(new Bundle{
//    val peconfig = Input(new PEConfigReg(16))
    val ram = Flipped(new RAMInterface())
    val done = Output(Bool())
    val oSumSRAM = Vec(32/2, DecoupledIO(SInt(8.W)))
//    val readGo = Input(Bool())
    // control regfile
    val regfile = new regfileInterface()
  })
  val ctrl = Module(new Controller(faddr=faddr, iaddr=iaddr))
  val global_reset = WireInit(reset)
  val pea = withReset(global_reset.asBool() | ctrl.io.peaReset){Module(new PEArray((3, 32)))}
  val pool = withReset(global_reset.asBool() | ctrl.io.peaReset){Module(new maxPooling())}
  val regfile = Module(new RegFile())
  pea.io.dataIn <> ctrl.io.dout
  pea.io.bias <> ctrl.io.bias
  pea.io.stateSW := ctrl.io.stateSW

  regfile.io.we := io.regfile.we
  regfile.io.raddr := io.regfile.raddr
  regfile.io.waddr := io.regfile.waddr
  regfile.io.din := io.regfile.din
  io.regfile.dout := regfile.io.dout
  pea.io.peconfig := regfile.io.peconfig
  ctrl.io.peconfig := regfile.io.peconfig
  ctrl.io.loop := regfile.io.loop

  ctrl.io.onceDone := pool.io.allDone
//  pea.io.peconfig := io.peconfig
  pool.io.channelOutNum := regfile.io.peconfig.filterNum

  ctrl.io.dataDone := pea.io.dataDone
  ctrl.io.ram <> io.ram
  ctrl.io.readGo := regfile.io.go

  pool.io.peaDone := pea.io.done
//  io.done := pool.io.allDone
  io.done := ctrl.io.allDone
  pea.io.oSumSRAM <> pool.io.din
  io.oSumSRAM <> pool.io.dout
  ctrl.io.oSumSRAM <> pool.io.dout
}

class TB(val faddr:Int = 0x0000, val iaddr:Int = 0x0480) extends Module{
  val io = IO(new Bundle{
//    val peconfig = Input(new PEConfigReg(16))
    val done = Output(Bool())
    val oSumSRAM = Vec(32/2, DecoupledIO(SInt(8.W)))
//    val readGo = Input(Bool())
    val dataCheck = Vec(32/2, Output(SInt(8.W)))
    val regfile = new regfileInterface()
  })
  val top = Module(new Top(faddr=faddr, iaddr=iaddr))
  val ram  = Module(new RAM)

  top.io.regfile <> io.regfile

  top.io.ram <> ram.io
  io.oSumSRAM <> top.io.oSumSRAM
  io.done := top.io.done

  val cnt = RegInit(0.U)
  val raddr = RegInit(0x8000.asUInt())
  when(io.done === true.B){
    cnt := 16.U
  }
  for (i <- 0 until io.dataCheck.length){
    io.dataCheck(i) := 0.S
  }
  when(cnt =/= 0.U){
    ram.io.raddr := raddr
    raddr := raddr + 1.U
    cnt := cnt - 1.U
    for (i <- 0 until io.dataCheck.length){
      io.dataCheck(i) := ram.io.dout(i)
    }
  }


}
