package myutil

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import chisel3.experimental._
import pe._
import myutil._
import breeze.linalg._
import config._

class Acc(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(SInt(p(OSumW).W)))
    val bias = Flipped(DecoupledIO(SInt(p(BiasW).W)))
    val out = DecoupledIO(SInt(p(AccW).W))
    val last = Input(Bool())
  })

  val reg = RegInit(0.asSInt(p(AccW).W))
  val qIn = Wire(DecoupledIO(SInt(p(AccW).W)))
  val q = Queue(qIn)
  qIn.valid := 0.U
  qIn.bits := 0.S
  io.out <> q

  when(io.in.fire()) {
    reg := reg + io.in.bits
  }.elsewhen(io.last) {
    when(io.bias.fire()) {
      qIn.valid := 1.U
      qIn.bits := reg + io.bias.bits
    }
  }
  io.in.ready := 1.U
  io.bias.ready := io.last
}
