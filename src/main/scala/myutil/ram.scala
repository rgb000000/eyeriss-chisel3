package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.util.experimental.loadMemoryFromFile

class ram_sim(val aw: Int, val dw: Int) extends BlackBox(Map("aw" -> aw, "dw" -> dw)) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val we = Input(Bool())
    val addr = Input(UInt(aw.W))
//    val waddr = Input(UInt(aw.W))
    val din = Input(UInt(dw.W))
    val dout = Output(UInt(dw.W))
  })
  setResource("/ram_sim.v")
  //  setResource("/ram.mem")
}

class RAM(val aw: Int = 20, val dw: Int=280) extends Module {
  val io = IO(new Bundle {
    val we = Input(Bool())
    val addr = Input(UInt(aw.W))
//    val waddr = Input(UInt(aw.W))
    val din = Input(Vec(dw / 8, SInt(8.W)))
    val dout = Output(Vec(dw / 8, SInt(8.W)))
  })
  val u = Module(new ram_sim(aw, dw))
  u.io.clk := clock
  u.io.we := io.we
  u.io.addr := io.addr
//  u.io.waddr := io.waddr
  u.io.din := io.din.asUInt()
  //  io.dout := u.io.dout
  for (i <- 0 until dw / 8) {
    io.dout(i) := u.io.dout(i * 8 + 7, i * 8).asSInt()
  }

  def read(addr: UInt): Vec[SInt] = {
    io.addr := addr
    io.we := false.B
    io.dout
  }

  def write(addr: UInt, data: SInt): Unit = {
    io.addr := addr
    io.we := true.B
    io.din := data
  }
}

class MemChisel(aw: Int, dw: Int) extends Module {
  val io = IO(new Bundle {
    val raddr = Input(UInt(aw.W))
    val waddr = Input(UInt(aw.W))
    val din = Input(SInt(dw.W))
    val dout = Output(SInt(dw.W))
    val we = Input(Bool())
  })

  val mem = Mem(2048, SInt(dw.W).cloneType)
  loadMemoryFromFile(mem, "ram.mem")

  io.dout := mem.read(io.raddr)
  when(io.we) {
    mem.write(io.waddr, io.din)
  }
}