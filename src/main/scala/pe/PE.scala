package pe

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import myutil._


//class PE(filterSpadLen: Int, imgSpadLen: Int, w: Int) extends Module {
//  val io = IO(new Bundle {
//    // 00 -> idle
//    // 01 -> getData
//    // 10 -> cal
//    val stateSW = Input(UInt(2.W))
//    val filter = Flipped(DecoupledIO(UInt(w.W)))
//    val img = Flipped(DecoupledIO(UInt(w.W)))
//    val pSumIn = Flipped(DecoupledIO(UInt(w.W)))
//    val oSum = DecoupledIO(UInt(w.W))
//    val regConfig = Input(UInt(16.W))
//  })
//
//  val idle :: getData :: cal :: Nil = Enum(3)
//  val state = RegInit(idle)
//  // reg 0 -> number of filter
//  // reg 1 -> number of img
//  // reg 2 -> number of channel
//  // reg 3 -> no use
//  val reg = RegInit(Vec(4, UInt(4.W)))
//
//
//  // Counter
//  val fcnt = Counter(filterSpadLen)
//  val icnt = Counter(imgSpadLen)
//  val fcntFull = !(fcnt.value === filterSpadLen.U)
//  val icntFull = !(icnt.value === imgSpadLen.U)
//  when(io.filter.fire()){
//    fcnt.inc()
//  }
//  when(io.img.fire()){
//    icnt.inc()
//  }
//
//  io.filter.ready := fcntFull & (state === getData)
//  io.img.ready := icntFull & (state === getData)
//
//  val doGetData = state === getData
//  val doCal = state === cal
//
//  val filterIn = Wire(io.filter.cloneType)
//  filterIn.bits := io.filter.bits
//  filterIn.valid := Mux(doGetData, io.filter.valid, 0.U(1.W))
//  filterIn.ready := Mux(doGetData, io.filter.ready, 0.U(1.W))
//  val imgIn = Wire(io.img.cloneType)
//  imgIn.bits := io.img.bits
//  imgIn.valid := Mux(doGetData, io.img.valid, 0.U(1.W))
//  imgIn.ready := Mux(doGetData, io.img.ready, 0.U(1.W))
//
//  val fQ = FIFO(filterIn, filterSpadLen)
//
//  fQ.ready := doCal
//  val iQ = FIFO(imgIn, imgSpadLen)
//  iQ.ready := doCal
//
//  val pSum = Wire(DecoupledIO(iQ.bits.cloneType))
//  pSum.valid := 0.U
//  pSum.bits := 0.U
//  val pSumQ = FIFO(pSum, 16)
//
//
//  switch(state){
//    is (idle){
//      when( (io.filter.valid | io.img.valid) & io.stateSW === 1.U){
//        state := getData
//      } .otherwise{
//        state := state
//      }
//    }
//    is (getData) {
//      // TODO to judge when to switch state to cal
//      when(!io.filter.valid & io.stateSW === 2.U){
//        state := cal
//      }.otherwise{
//        state := state
//      }
//      reg(3) := io.regConfig(15,12)
//      reg(2) := io.regConfig(11,8)
//      reg(1) := io.regConfig(7,4)
//      reg(0) := io.regConfig(3,0)
//    }
//    is (cal){
//      when(iQ.fire() & fQ.fire()){
//        pSum.bits := fQ.bits * iQ.bits
//        pSum.valid := 1.U
//      }.otherwise{
//        pSum.valid := 0.U
//        pSum.bits := 0.U
//      }
//    }
//  }
//  io.oSum <> pSumQ
//}

@chiselName
class PE(filterSpadLen: Int = 225, imgSpadLen: Int = 12, w:Int = 16) extends Module{
    val io = IO(new Bundle {
      // 00 -> idle
      // 01 -> getData
      // 10 -> cal
      val stateSW = Input(UInt(2.W))
      val regConfig = Input(UInt(16.W))

      val filter = Flipped(DecoupledIO(UInt(w.W)))
      val img = Flipped(DecoupledIO(UInt(w.W)))
      val pSumIn = Flipped(DecoupledIO(UInt(w.W)))
      val oSum = DecoupledIO(UInt(w.W))
    })

  io.pSumIn.ready := 0.U
  io.oSum.valid := 0.U
  io.oSum.bits := 0.U

  val idle :: data :: cal :: done :: Nil = Enum(4)
  val state = RegInit(idle)
  val dodata = WireInit(state === data)
  val docal = WireInit(state === cal)

  val fQMuxIn = Wire(DecoupledIO(io.filter.bits.cloneType))
  val fQ = FIFO(fQMuxIn, filterSpadLen)
  io.filter.ready := 0.U
  fQ.ready := 0.U
  when(dodata){
    fQMuxIn :=  io.filter
  }.otherwise{
    fQMuxIn := fQ
  }
  fQ.ready := state === cal

  val iQMuxIn = Wire(DecoupledIO(io.img.bits.cloneType))
  val iQ = FIFO(iQMuxIn, imgSpadLen)
  io.img.ready := 0.U
  iQ.ready := 0.U
  when(dodata){
    iQMuxIn := io.img
  }.otherwise{
    iQMuxIn := iQ
  }
  iQ.ready := state === cal

  val mulReg = RegInit(0.U(w.W))

  switch (state){
    is(idle){
      when(io.stateSW === dodata){
        state := data
      }
    }
    is(data){
      // to swith to cal, filter in must done
      when(io.stateSW === docal | io.filter.valid === 0.U){
        state := cal
      }
    }
    is(cal){
      when(fQ.fire() & iQ.fire()){
        io.oSum.valid := 1.U
        io.oSum.bits := fQ.bits + iQ.bits
      }.otherwise{

      }
    }
    is(done){

    }
  }


}
