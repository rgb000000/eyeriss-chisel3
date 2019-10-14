package myutil

import chisel3._
import pe.PEConfigReg
import node.dataPackage
import chisel3.experimental._
import chisel3.util._

class RAMInterface(val aw: Int = 20, val dw: Int = 280) extends Bundle {
  val we = Input(Bool())
  val addr = Input(UInt(aw.W))
//  val waddr = Input(UInt(aw.W))
  val din = Input(Vec(dw / 8, SInt(8.W)))
  val dout = Output(Vec(dw / 8, SInt(8.W)))
}

class Controller(faddr: Int = 0x0000, iaddr: Int = 0x0480, waddr: Int = 4500,
                 aw: Int = 20, dw: Int = 280, w: Int = 8
                ) extends Module {
  val io = IO(new Bundle {
    val ram = Flipped(new RAMInterface(aw, dw))
    //    val config = Input(new PEConfigReg)
    val dout = Decoupled(new dataPackage(w))
    val bias = DecoupledIO(SInt((w * 2).W))
    val stateSW = Output(UInt(2.W))
    val readGo = Input(Bool())
    val dataDone = Input(Bool())
    val oSumSRAM = Vec(32 / 2, Flipped(DecoupledIO(SInt(8.W))))
    val peconfig = Input(new PEConfigReg())
    val loop = Input(UInt(8.W))
    val peaReset = Output(Bool())
    val onceDone = Input(Bool())
    val allDone = Output(Bool())
  })

  val faddr_reg = RegInit(faddr.asUInt(aw.W))
  val iaddr_reg = RegInit(iaddr.asUInt(aw.W))

  val idle :: filter :: rbias :: img :: end :: allEnd :: Nil = Enum(6)
  val state = RegInit(idle)
  val qin = Wire(io.dout.cloneType)
  qin.bits.data := 0.U
  qin.bits.dataType := 0.U
  qin.bits.cnt := 0.U
  qin.bits.positon.row := 0.U
  qin.bits.positon.col := 0.U
  qin.valid := 0.U
  val q = Queue(qin, 4)
  io.dout <> q


  val stateSW = RegInit(0.asUInt(2.W))
  io.stateSW := stateSW

  val total_filter = RegInit(0.asUInt(16.W))
  val total_img = RegInit(0.asUInt(16.W))
  dontTouch(total_filter)
  dontTouch(total_img)

  io.ram.we := false.B
  io.ram.addr := 0.U
//  io.ram.waddr := 0.U
  io.ram.din.foreach(_ := 0.S)
  io.oSumSRAM.map(_.ready).foreach(_ := 0.U)
  val waddr_reg = RegInit(waddr.asUInt(20.W))
  val ram_raddr_valid = WireInit(0.U(1.W))
  ram_raddr_valid := 0.U
  val ram_rdata_valid = RegNext(ram_raddr_valid)
  // write logic
//  when(io.oSumSRAM.map(_.fire()).reduce(_ & _)) {
//    io.ram.we := 1.U
//    io.ram.addr := waddr_reg
//    (io.ram.din, io.oSumSRAM).zipped.foreach(_ := _.bits)
//    waddr_reg := waddr_reg + 1.U
//  }


  // read logic
  val fcnt = Counter(192)
  val fchannel = Counter(256)
  val fNum = Counter(256)
  val fLen = Counter(256)

  val icnt = Counter(192)
  val data_cnt = Reg(UInt(8.W))
  val row_reg = Reg(UInt(8.W))
  val col_reg = Reg(UInt(8.W))

  val bias = Reg(SInt((2 * w).W))
  val biasqIn = Wire(DecoupledIO(SInt((2 * w).W)))
  val biasq = Queue(biasqIn, 32)
  io.bias <> biasq
  biasqIn.bits := 0.S
  biasqIn.valid := 0.U

  val loop = Reg(UInt(8.W))
  val curLoop = RegInit(0.asUInt(8.W))
  io.peaReset := false.B

  io.allDone := 0.U

  switch(state) {
    is(idle) {
      row_reg := 0.U
      col_reg := 0.U
      data_cnt := 0.U
      loop := io.loop
      when(io.readGo) {
        state := filter
        stateSW := 1.U // pearray get data
      }
    }
    is(filter) {
      stateSW := 1.U
      qin.valid := ram_rdata_valid
      io.ram.addr := faddr_reg
      ram_raddr_valid := 1.U
      faddr_reg := faddr_reg + 1.U
      when(qin.fire()) {
        total_filter := total_filter + 1.U
        fcnt.inc()
        when(fNum.value === io.peconfig.filterNum - 1.U) {
          fNum.value := 0.U
          when(fchannel.value === io.peconfig.nchannel - 1.U) {
            fchannel.value := 0.U
            when(fLen.value === io.peconfig.singleFilterLen - 1.U) {
              fLen.value := 0.U
              when(row_reg === (io.peconfig.singleFilterLen - 1.U).asUInt()) {
                state := rbias
                ram_raddr_valid := 0.U
                faddr_reg := faddr_reg
                row_reg := 0.U
                col_reg := 0.U
                fcnt.value := 0.U

                fNum.value := 0.U
                fchannel.value := 0.U
                fLen.value := 0.U

              }.otherwise {
                row_reg := row_reg + 1.U
                fcnt.value := 0.U
              }
            }.otherwise {
              fLen.inc()
            }
          }.otherwise {
            fchannel.inc()
          }
        }.otherwise {
          fNum.inc()
        }
        // read bias
        //        when((fchannel.value === io.peconfig.nchannel - 1.U) &
        //          (row_reg === 2.S) &
        //          (fLen.value === io.peconfig.singleFilterLen - 1.U)){
        //          val tmp = Wire(new Bundle{
        //            val high = UInt(w.W)
        //            val low = UInt(w.W)
        //          })
        //          tmp.high := qin.bits.data(34).asUInt()
        //          tmp.low := qin.bits.data(33).asUInt()
        //          biasqIn.bits := tmp.asUInt().asSInt()
        //          biasqIn.valid := 1.U
        //        }
      }
      qin.bits.dataType := 0.U
      qin.bits.data := io.ram.dout(curLoop).asUInt()
      qin.bits.cnt := 1.U
      qin.bits.positon.col := 1.U
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
    is(rbias) {
      // read bias
      //        when((fchannel.value === io.peconfig.nchannel - 1.U) &
      //          (row_reg === 2.S) &
      //          (fLen.value === io.peconfig.singleFilterLen - 1.U)){
      //          val tmp = Wire(new Bundle{
      //            val high = UInt(w.W)
      //            val low = UInt(w.W)
      //          })
      //          tmp.high := qin.bits.data(34).asUInt()
      //          tmp.low := qin.bits.data(33).asUInt()
      //          biasqIn.bits := tmp.asUInt().asSInt()
      //          biasqIn.valid := 1.U
      //        }
      when(curLoop > 15.U) {
        io.ram.addr := faddr_reg + 4.U
      }.otherwise {
        io.ram.addr := faddr_reg
      }
      ram_raddr_valid := 1.U
      faddr_reg := faddr_reg + 1.U
      when(ram_rdata_valid === 1.U) {
        val tmp = Wire(new Bundle {
          val high = UInt(w.W)
          val low = UInt(w.W)
        })
        tmp.high := io.ram.dout(((curLoop%16.U) << 1.U).asUInt() + 1.U).asUInt()
        tmp.low := io.ram.dout(((curLoop%16.U) << 1.U).asUInt()).asUInt()
        biasqIn.bits := tmp.asUInt().asSInt()
        biasqIn.valid := 1.U
        when(fcnt.value === io.peconfig.filterNum - 1.U){
          fcnt.value := 0.U
          ram_raddr_valid := 0.U
          state := img
        }.otherwise{
          fcnt.inc()
        }
      }
    }
    is(img) {
      when(io.dataDone) {
        stateSW := 2.U //pearray start cal
      }
      qin.valid := ram_rdata_valid
      io.ram.addr := iaddr_reg
      when(qin.ready) {
        ram_raddr_valid := 1.U
        iaddr_reg := iaddr_reg + 1.U
      }.otherwise {
        io.oSumSRAM.map(_.ready).foreach(_ := 1.U)
        when(io.oSumSRAM.map(_.fire()).reduce(_ & _)) {
          io.ram.we := 1.U
          io.ram.addr := waddr_reg
          (io.ram.din, io.oSumSRAM).zipped.foreach(_ := _.bits)
          waddr_reg := waddr_reg + 1.U
          ram_raddr_valid := 0.U
        }.otherwise{
          ram_raddr_valid := 1.U
          io.ram.addr := iaddr_reg - 1.U
        }
      }
      when(qin.fire()) {
        total_img := total_img - 1.U

        //        when(fchannel.value === io.peconfig.nchannel - 1.U){
        //          fchannel.value := 0.U
        //          when(fLen.value === io.peconfig.singleImgLen - 1.U){
        //            fLen.value := 0.U
        //            when(fNum.value === io.peconfig.imgNum - 1.U){
        //              when(row_reg === (io.peconfig.singleImgLen - 1.U).asSInt()){
        //                row_reg := 0.U
        //                state := end
        //              }.otherwise{
        //                row_reg := row_reg + 1.S
        //              }
        //            }.otherwise{
        //              fNum.inc()
        //            }
        //          }.otherwise{
        //            fLen.inc()
        //          }
        //        }.otherwise{
        //          fchannel.inc()
        //        }
        when(fNum.value === io.peconfig.singleImgLen - 1.U) {
          fNum.value := 0.U
          when(fLen.value === io.peconfig.nchannel - 1.U) {
            fLen.value := 0.U
            fNum.value := 0.U
            row_reg := 0.U
            state := end
            ram_raddr_valid := 0.U
          }.otherwise {
            fLen.inc()
          }
        }.otherwise {
          fNum.inc()
        }

        fcnt.inc()
        //        when(total_img === 1.U){
        //          state := end
        //        }
        when(row_reg === 33.U) {
          row_reg := 0.U
        }.otherwise {
          row_reg := row_reg + 1.U
        }

      }
      qin.bits.dataType := 1.U
      qin.bits.data := io.ram.dout.asUInt()
      qin.bits.cnt := 34.U
      qin.bits.positon.col := 0.U
      qin.bits.positon.row := row_reg
    }
    is(end) {
      io.oSumSRAM.map(_.ready).foreach(_ := 1.U)
      when(io.oSumSRAM.map(_.fire()).reduce(_ & _)) {
        io.ram.we := 1.U
        io.ram.addr := waddr_reg
        (io.ram.din, io.oSumSRAM).zipped.foreach(_ := _.bits)
        waddr_reg := waddr_reg + 1.U
      }
      when(io.onceDone) {
        when(loop === 1.U) {
          state := allEnd
          io.allDone := 1.U
        }.otherwise {
          iaddr_reg := iaddr.asUInt(aw.W)
          faddr_reg := faddr.asUInt(aw.W)
          stateSW := 0.U
          state := filter
          io.peaReset := true.B
          loop := loop - 1.U
          curLoop := curLoop + 1.U
        }
      }
    }
    is(allEnd) {
//      io.allDone := 1.U
    }
  }


}
