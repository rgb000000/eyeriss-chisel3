// BWN
package top

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import chisel3.experimental._
import pe._
import myutil._
import breeze.linalg._
import config._
import axi._
import node._

class BWN(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val host = new AXILiteClient(p(ShellKey).hostParams)
    val mem = new AXIMaster(p(ShellKey).memParams)
  })

  val pearray = Module(new PEArray2Top)
  val vme = Module(new VME)
  val regfile = Module(new AXIRegFile)

}
