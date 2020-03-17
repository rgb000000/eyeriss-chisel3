package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._

class widthConvert(implicit p: Parameters) extends Module{
  val inWidth = p(OSumW)
  val outWidth = p(ShellKey).memParams.dataBits
  val nums: Int = (outWidth/inWidth).toInt
  val io = IO(new Bundle{
    val in = Flipped(DecoupledIO(SInt(inWidth.W)))
    val out = DecoupledIO(Vec(nums, SInt(inWidth.W)))
  })

  val cnt = Counter(nums)
  val regs = Reg(Vec(nums, SInt(inWidth.W)))

  io.in.ready := io.out.ready

  when(io.in.fire()){
    cnt.inc()
    regs(cnt.value) := io.in.bits
  }
  io.out.valid := (cnt.value === (nums-1).asUInt()) & (io.in.fire())
  // last should be wire
  (io.out.bits, regs).zipped.foreach(_ := _)
  io.out.bits(nums-1) := io.in.bits
}
