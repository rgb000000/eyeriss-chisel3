package myutil

import chisel3._
import chisel3.util._

import scala.collection.script.Reset

class DW_ram_r_w_s_dff extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rst_n = Input(UInt(1.W))
    val cs_n = Input(UInt(1.W))
    val wr_n = Input(UInt(1.W))
    val rd_addr = Input(UInt(8.W))
    val wr_addr = Input(UInt(8.W))
    val data_in = Input(SInt(16.W))
    val data_out = Output(SInt(16.W))
  })
  setResource("/DW_ram_r_w_s_dff.v")
}

class MySRAM(len: Int = 8) extends Module {
  val io = IO(new Bundle {
    val we = Input(UInt(1.W))
    val addr = Input(UInt(8.W))
    val din = Input(SInt(16.W))
    val dout = Output(SInt(16.W))
    val rstLowas = Input(Bool())
  })
  withReset(io.rstLowas) {
    val regfile = RegInit(VecInit(Seq.fill(len)(0.S(16.W))))
    // read
    io.dout := regfile(io.addr)
    when(io.we === 0.U) {
      // write
      regfile(io.addr) := io.din
    }
  }

  def read(addr: UInt): SInt = {
    io.addr := addr
    io.we := 1.U
    io.dout
  }

  def write(addr: UInt, data: SInt): Unit = {
    io.addr := addr
    io.we := 0.U
    io.din := data
  }
}

class SRAM extends Module {
  val io = IO(new Bundle {
    val we = Input(UInt(1.W))
    val addr = Input(UInt(8.W))
    val din = Input(SInt(16.W))
    val dout = Output(SInt(16.W))
    val rstLowas = Input(UInt(1.W))
  })
  val ram = Module(new DW_ram_r_w_s_dff())
  ram.io.clk := clock
  ram.io.rst_n := io.rstLowas
  ram.io.cs_n := 0.U
  ram.io.wr_n := io.we
  ram.io.rd_addr := io.addr
  ram.io.wr_addr := io.addr
  ram.io.data_in := io.din
  io.dout := ram.io.data_out

  def read(addr: UInt): SInt = {
    io.addr := addr
    io.we := 1.U
    io.dout
  }

  def write(addr: UInt, data: SInt): Unit = {
    io.addr := addr
    io.we := 0.U
    io.din := data
  }
}

object SRAM {
  def apply: SRAM = new SRAM()
}
