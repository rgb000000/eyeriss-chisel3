package node

import chisel3._
import chisel3.util._
import pe._

class testPlatform extends Module{
  val io = IO(new Bundle{
    val stateSW = Input(UInt(2.W))
    val done = Output(UInt(1.W))
  })

  // aim to 32*32    padding = 1    kernel 3*3
  val PEArray = Module(new PEArray((3, 32), 8))

  // peconfig
  val peconfig = Wire(new PEConfigReg())
  peconfig.filterNum := 1.U
  peconfig.singleFilterLen := 3.U
  peconfig.imgNum := 1.U
  peconfig.singleImgLen := 34.U
  peconfig.nchannel := 64.U
  peconfig.relu := 1.U

}
