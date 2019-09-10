package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import node._

class handleStop extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new dataPackage(8).cloneType))
    val stop = Input(Bool())
    val dataOut = DecoupledIO(new dataPackage(8).cloneType)
  })

  io.dataIn.ready := io.dataOut.ready & (!io.stop)
  io.dataOut.valid := io.dataIn.valid & (!io.stop)
  io.dataOut.bits := io.dataIn.bits

}


