package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import pe.PEConfigReg
import axi._

class RegFile(val len:Int = 8, val aw:Int = 3, val dw:Int = 8) extends Module{
  val io = IO(new Bundle{
    val we = Input(Bool())
    val addr = Input(UInt(aw.W))
//    val waddr = Input(UInt(aw.W))
    val din = Input(UInt(8.W))
    val dout = Output(UInt(8.W))
    val peconfig = Output(new PEConfigReg(dw))
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

