package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._
import pe.{PEConfigReg}

class ImgReader(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val reader = new VMEReadMaster
    val dout = Output(Vec(p(Shape)._1 + p(Shape)._2 - 1, DecoupledIO(UInt(p(ShellKey).memParams.dataBits.W))))

    val addr = Input(UInt(p(ShellKey).memParams.addrBits.W))
    val peconfig = Input(new PEConfigReg)

    val go = Input(Bool())
  })

  val idle :: work :: Nil = Enum(2)
  val state = RegInit(idle)
  val addr = Reg(io.addr.cloneType)

  switch(state){
    is(idle){
      state := work
    }
    is(work){

    }
  }

}
