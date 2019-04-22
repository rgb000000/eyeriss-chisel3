package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._

class DW_ram_r_w_s_dff extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle{
    val clk = Input(Clock())
    val rst_n = Input(UInt(1.W))
    val cs_n = Input(UInt(1.W))
    val wr_n = Input(UInt(1.W))
    val rd_addr = Input(UInt(8.W))
    val wr_addr = Input(UInt(8.W))
    val data_in = Input(SInt(16.W))
    val data_out = Output(SInt(16.W))
  })
  setResource("/DW_ram_r_w_s_dff.v")

//  def write(addr:UInt, data:SInt): Unit ={
//    io.wea := 1.U
//    io.addra := addr
//    io.dina := data
//  }
//  def read(addr:UInt): SInt = {
//    io.wea := 0.U
//    io.addra := addr
//    io.douta
//  }
}

//object mySram{
//  def apply: mySram = new mySram()
//}
