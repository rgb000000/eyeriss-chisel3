package myutil

import chisel3._
import pe.PEConfigReg
import node.dataPackage
import chisel3.experimental._
import chisel3.util._

class RAMInterface(val aw:Int=20, val dw:Int=280) extends Bundle{
  val we = Input(Bool())
  val raddr = Input(UInt(aw.W))
  val waddr = Input(UInt(aw.W))
  val din = Input(Vec(dw / 8, SInt(8.W)))
  val dout = Output(Vec(dw / 8, SInt(8.W)))
}

class Controller(faddr:Int = 0x0000, iaddr:Int = 0x240, aw:Int=20, dw:Int=280, w:Int = 8) extends Module{
  val io = IO(new Bundle{
    val ram = Flipped(new RAMInterface(aw, dw))
//    val config = Input(new PEConfigReg)
    val dout = Decoupled(new dataPackage(w))
    val bias = Output(SInt(w.W))
    val stateSW = Output(UInt(2.W))
    val readGo = Input(Bool())
    val dataDone = Input(Bool())
  })
  val faddr_reg = RegInit(faddr.asUInt(16.W))
  val iaddr_reg = RegInit(iaddr.asUInt(16.W))

  val idle :: filter :: img :: end :: Nil = Enum(4)
  val state = RegInit(idle)
  val qin = Wire(io.dout.cloneType)
  qin.bits.data.foreach(_ := 0.S)
  qin.bits.dataType := 0.U
  qin.bits.cnt := 0.U
  qin.bits.positon.row := 0.S
  qin.bits.positon.col := 0.S
  qin.valid := 0.U
  val q = Queue(qin, 4)
  io.dout <> q


  val stateSW = RegInit(0.asUInt(2.W))
  io.stateSW := stateSW

  val total_filter = RegInit(576.asUInt())
  val total_img = RegInit(2176.asUInt())

  io.ram.we := false.B
  io.ram.raddr := 0.U
  io.ram.waddr := 0.U
  io.ram.din.foreach(_ := 0.S)

  val fcnt = Counter(192)
  val icnt = Counter(192)
  val data_cnt = Reg(UInt(8.W))
  val row_reg = Reg(SInt(8.W))
  val col_reg = Reg(SInt(8.W))

  val bias = Reg(SInt(8.W))
  io.bias := bias

  switch(state){
    is(idle){
      row_reg := 0.S
      col_reg := 0.S
      data_cnt := 0.U
      when(io.readGo){
        state := filter
        stateSW := 1.U  // pearray get data
      }
    }
    is(filter){
      qin.valid := 1.U
      io.ram.raddr := faddr_reg
      when(qin.fire()){
        faddr_reg := faddr_reg + 1.U
        total_filter := total_filter - 1.U
        fcnt.inc()
      }
      qin.bits.dataType := 0.U
      qin.bits.data := io.ram.dout
      qin.bits.cnt := 1.U
      qin.bits.positon.col := (-1).S
      qin.bits.positon.row := row_reg
      when(fcnt.value === 191.U){
        when(row_reg === 2.S){
          state := img
          row_reg := 0.S
          col_reg := 0.S
          fcnt.value := 0.U
          bias := qin.bits.data(34)
        }.otherwise{
          row_reg := row_reg + 1.S
          fcnt.value := 0.U
        }
      }
    }
    is(img){
      when(io.dataDone){
        stateSW := 2.U    //pearray start cal
      }
      qin.valid := 1.U
      io.ram.raddr := iaddr_reg
      when(qin.fire()){
        iaddr_reg := iaddr_reg + 1.U
        total_img := total_img - 1.U
        fcnt.inc()
        when(total_img === 1.U){
          state := end
        }
        when(row_reg === 33.S){
          row_reg := 0.S
        }.otherwise{
          row_reg := row_reg + 1.S
        }
      }
      qin.bits.dataType := 1.U
      qin.bits.data := io.ram.dout
      qin.bits.cnt := 34.U
      qin.bits.positon.col := 1.S
      qin.bits.positon.row := row_reg
    }
    is(end){

    }
  }


}
