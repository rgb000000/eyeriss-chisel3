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

class Controller(faddr:Int = 0x0000, iaddr:Int = 0x0480, waddr:Int = 0x8000,
                 aw:Int=20, dw:Int=280, w:Int = 8,
                 singleFilterNum:Int = 3*64
                ) extends Module{
  val io = IO(new Bundle{
    val ram = Flipped(new RAMInterface(aw, dw))
//    val config = Input(new PEConfigReg)
    val dout = Decoupled(new dataPackage(w))
    val bias = DecoupledIO(SInt((w*2).W))
    val stateSW = Output(UInt(2.W))
    val readGo = Input(Bool())
    val dataDone = Input(Bool())
    val oSumSRAM = Vec(32/2, Flipped(DecoupledIO(SInt(8.W))))
    val peconfig = Input(new PEConfigReg())
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
  io.oSumSRAM.map(_.ready).foreach(_ := 1.U)
  val waddr_reg = RegInit(waddr.asUInt(16.W))
  // write logic
  when(io.oSumSRAM.map(_.fire()).reduce(_ & _)){
    io.ram.we := 1.U
    io.ram.waddr := waddr_reg
    (io.ram.din, io.oSumSRAM).zipped.foreach(_ := _.bits)
    waddr_reg := waddr_reg + 1.U
  }



  // read logic
  val fcnt = Counter(192)
  val fchannel = Counter(256)
  val fNum = Counter(256)
  val fLen = Counter(256)

  val icnt = Counter(192)
  val data_cnt = Reg(UInt(8.W))
  val row_reg = Reg(SInt(8.W))
  val col_reg = Reg(SInt(8.W))

  val bias = Reg(SInt((2*w).W))
  val biasqIn = Wire(DecoupledIO(SInt((2*w).W)))
  val biasq = Queue(biasqIn, 8)
  io.bias <> biasq
  biasqIn.bits := 0.S
  biasqIn.valid := 0.U

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
        when(fNum.value === io.peconfig.filterNum - 1.U){
          fNum.value := 0.U
          when(fchannel.value === io.peconfig.nchannel - 1.U){
            fchannel.value := 0.U
            when(fLen.value === io.peconfig.singleFilterLen - 1.U){
              fLen.value := 0.U
              when(row_reg === 2.S){
                state := img
                row_reg := 0.S
                col_reg := 0.S
                fcnt.value := 0.U
              }.otherwise{
                row_reg := row_reg + 1.S
                fcnt.value := 0.U
              }
            }.otherwise{
              fLen.inc()
            }
          }.otherwise{
            fchannel.inc()
          }
        }.otherwise{
          fNum.inc()
        }
        when((fchannel.value === io.peconfig.nchannel - 1.U) &
          (row_reg === 2.S) &
          (fLen.value === io.peconfig.singleFilterLen - 1.U)){
          val tmp = Wire(new Bundle{
            val high = UInt(w.W)
            val low = UInt(w.W)
          })
          tmp.high := qin.bits.data(34).asUInt()
          tmp.low := qin.bits.data(33).asUInt()
          biasqIn.bits := tmp.asUInt().asSInt()
          biasqIn.valid := 1.U
        }
      }
      qin.bits.dataType := 0.U
      qin.bits.data := io.ram.dout
      qin.bits.cnt := 1.U
      qin.bits.positon.col := (-1).S
      qin.bits.positon.row := row_reg


//      when(fcnt.value === singleFilterNum.asUInt()){
//        when(row_reg === 2.S){
//          state := img
//          row_reg := 0.S
//          col_reg := 0.S
//          fcnt.value := 0.U
//          val tmp = Wire(new Bundle{
//            val high = UInt(w.W)
//            val low = UInt(w.W)
//          })
//          tmp.high := qin.bits.data(34).asUInt()
//          tmp.low := qin.bits.data(33).asUInt()
//          biasqIn.bits := tmp.asUInt().asSInt()
//          biasqIn.valid := 1.U
//        }.otherwise{
//          row_reg := row_reg + 1.S
//          fcnt.value := 0.U
//        }
//      }

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
