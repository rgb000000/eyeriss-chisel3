package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._

class maxPooling(val cols:Int = 32, val stride:Int = 2) extends Module{
  val io = IO(new Bundle{
    val din = Vec(cols, Flipped(DecoupledIO(SInt(8.W))))
    val dout = Vec(cols/2, DecoupledIO(SInt(8.W)))
  })

  val ud :: lr :: Nil = Enum(2)

  val din = io.din.grouped(2).toList
  for(i <- din.indices){
    val qIn = Wire(DecoupledIO(SInt(8.W)))
    val q = Queue(qIn, 2)
    val state = RegInit(ud)
    val out = Reg(SInt(8.W))
    din(i)(0).ready := qIn.ready
    din(i)(1).ready := qIn.ready
    io.dout(i) <> q
    qIn.bits := 0.S
    qIn.valid := 0.U
    switch(state){
      is(ud){
        when(din(i)(0).fire() & din(i)(1).fire()){
          out := Mux(din(i)(0).bits > din(i)(1).bits, din(i)(0).bits, din(i)(1).bits)
          state := lr
        }
      }
      is(lr){
        when(din(i)(0).fire() & din(i)(1).fire()){
          val tmp = Mux(din(i)(0).bits > din(i)(1).bits, din(i)(0).bits, din(i)(1).bits)
          qIn.bits := Mux(out > tmp, out , tmp)
          qIn.valid := 1.U
          state := ud
        }
      }
    }
  }


}
