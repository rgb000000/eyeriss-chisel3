package top

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import node._
import myutil._
import  pe._

class Top extends Module{
  val io = IO(new Bundle{
    val peconfig = Input(new PEConfigReg(16))
    val ram = Flipped(new RAMInterface())
    val done = Output(Bool())
    val oSumSRAM = Vec(32, DecoupledIO(SInt(8.W)))
    val readGo = Input(Bool())
  })
  val pea = Module(new PEArray((3, 32)))
  val ctrl = Module(new Controller())
  pea.io.dataIn <> ctrl.io.dout
  pea.io.bias := ctrl.io.bias
  pea.io.stateSW := ctrl.io.stateSW
  pea.io.peconfig := io.peconfig
  ctrl.io.dataDone := pea.io.dataDone
  ctrl.io.ram <> io.ram
  ctrl.io.readGo := io.readGo

  io.done := pea.io.done
  io.oSumSRAM <> pea.io.oSumSRAM
}

class TB extends Module{
  val io = IO(new Bundle{
    val peconfig = Input(new PEConfigReg(16))
    val done = Output(Bool())
    val oSumSRAM = Vec(32, DecoupledIO(SInt(8.W)))
    val readGo = Input(Bool())
  })
  val top = Module(new Top)
  val ram  = Module(new RAM)

  top.io.ram <> ram.io
  top.io.peconfig := io.peconfig
  top.io.readGo := io.readGo
  io.oSumSRAM <> top.io.oSumSRAM
  io.done := top.io.done

}
