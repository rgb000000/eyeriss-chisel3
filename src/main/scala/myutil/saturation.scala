package myutil

import chisel3._
import chisel3.util._

class Saturation(inw: Int = 26, outw: Int = 8, low: Int = 2) extends Module {
  val io = IO(new Bundle {
    val dataIn = Input(SInt(inw.W))
    val dataOut = Output(SInt(outw.W))
  })

  val yuanma = Wire(UInt((inw - 1).W))
  val sign = WireInit(io.dataIn(inw - 1))
  val slice = Wire(UInt((inw - 1).W))
  val carry = Wire(UInt(1.W))

  when(sign === 1.U) {
    // fu shu
    yuanma := (~io.dataIn(inw - 1, 0)).asUInt() + 1.U
  }.otherwise {
    // zheng shu
    yuanma := io.dataIn(inw - 1, 0)
  }
  slice := yuanma(inw - 1 - 1, low)

  when(yuanma(low - 1) === 1.U) {
    carry := 1.U
  }.otherwise {
    carry := 0.U
  }
  val out = Wire(new Bundle {
    val sign = UInt(1.W)
    val bits = UInt((outw - 1).W)
  })
  out.sign := 0.U
  out.bits := 0.U
  when(sign === 1.U) {
    when((slice + carry).asUInt() >= 128.U) {
      io.dataOut := (-128).asSInt(outw.W)
    }.otherwise {
      out.sign := sign
      out.bits := (~(slice + carry)).asUInt() + 1.U
      io.dataOut := out.asUInt().asSInt()
    }
  }.otherwise {
    when((slice + carry).asUInt() >= 127.U) {
      io.dataOut := 127.asSInt(outw.W)
    }.otherwise {
      out.sign := sign
      out.bits := (slice + carry).asUInt()
      io.dataOut := out.asUInt().asSInt()
    }
  }


}
