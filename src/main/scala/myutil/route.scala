package myutil

import chisel3._
import chisel3.util._

class Interface(val aw: Int = 20, val dw: Int = 280) extends Bundle {
  val we = Input(Bool())
  val addr = Input(UInt(aw.W))
  val din = Input(UInt(dw.W))
  val dout = Output(UInt(dw.W))
}

class Route() extends Module {
  val io = IO(new Bundle {
    val left = new Interface(8, 32)
    val right1 = Flipped(new Interface(8, 32))
    val right2 = Flipped(new Interface(3, 8))
    val right3 = Flipped(new Interface(6, 32))
  })
  io.right2.we := 0.U
  io.right2.addr := 0.U
  io.right2.din := 0.U

  io.right1.we := 0.U
  io.right1.addr := 0.U
  io.right1.din := 0.U

  io.right3.we := 0.U
  io.right3.addr := 0.U
  io.right3.din := 0.U

  io.left.dout := 0.U

  when(io.left.addr(7, 6) === 0.U) {
    // system reg
    io.left <> io.right1
  }.elsewhen(io.left.addr(7, 6) === 1.U) {
    // acc reg
    val tmp = Wire(new Interface(3, 8))
    tmp.we := io.left.we
    tmp.addr := io.left.addr(2, 0).asUInt()
    tmp.din := io.left.din(7, 0)
    io.left.dout := tmp.dout
    tmp <> io.right2
  }.elsewhen(io.left.addr(7, 6) === 2.U) {
    // bram reg
    val tmp = Wire(new Interface(6, 32))
    tmp.we := io.left.we
    tmp.addr := io.left.addr(5, 0).asUInt()
    tmp.din := io.left.din
    io.left.dout := tmp.dout
    tmp <> io.right3
  }
}

class BramController extends Module {
  val io = IO(new Bundle {
    val bramio = Flipped(new Interface(13, 280))
    val in = new Interface(6, 32)
  })
  //  val addr = RegInit(0.U(32.W))
  //  val tobram = RegInit(VecInit(Seq.fill(35)(0.U(8.W))))
  //  val frombram = RegInit(VecInit(Seq.fill(35)(0.U(8.W))))
  //  val we = RegInit(0.U(1.W))
  // 0 addr
  // 1 we
  // 2 to 36 tobram
  val regfile = RegInit(VecInit(Seq.fill(37)(0.U(32.W))))
  io.bramio.addr := regfile(0) >> 4
  io.bramio.we := regfile(1)
  val tobram = Wire(Vec(35, UInt(8.W)))
  for (i <- tobram.indices) {
    tobram(i) := regfile(2 + i)
  }
  io.bramio.din := tobram.asUInt()

  val frombram = RegInit(VecInit(Seq.fill(35)(0.asUInt(8.W))))
  for (i <- frombram.indices) {
    frombram(i) := io.bramio.dout((i + 1) * 8 - 1, i * 8)
  }

  when(io.in.we){
    regfile(io.in.addr) := io.in.din
  }
  when(regfile(0)(3,0) <= 0x1000.U){
    io.in.dout := frombram(regfile(0)(3,0))
  }.otherwise{
    io.in.dout := 0xf0f0.U
  }
}