package pe

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import myutil._



class PEConfigReg(val w:Int = 16) extends Bundle{
  val filterNum = UInt(w.W)
  val imgNum = UInt(w.W)
  val nchannel = UInt(w.W)
}


// take care!  in PE stateSw is buffer one time
@chiselName
class PE(filterSpadLen: Int = 225, imgSpadLen: Int = 225, w:Int = 16) extends Module{
    val io = IO(new Bundle {
      // 00 -> idle
      // 01 -> getData
      // 10 -> cal
      val stateSW = Input(UInt(2.W))
      val regConfig = Input(new PEConfigReg(16))

      val filter = Flipped(DecoupledIO(UInt(w.W)))
      val img = Flipped(DecoupledIO(UInt(w.W)))
      val pSumIn = Flipped(DecoupledIO(UInt(w.W)))
      val oSum = DecoupledIO(UInt(w.W))
    })

  val configReg = Reg(new PEConfigReg(16))

  io.pSumIn.ready := 0.U
  io.oSum.valid := 0.U
  io.oSum.bits := 0.U

  val fCnt = Counter(256)

  // data mean getData
  val idle :: data :: cal :: pDone :: allDone:: Nil = Enum(5)
  val state = RegInit(idle)
  val dodata = WireInit(state === data)
  val docal = WireInit(state === cal)


  val fQMuxIn = Wire(DecoupledIO(io.filter.bits.cloneType))
  val fQ = FIFO(fQMuxIn, filterSpadLen)
  io.filter.ready := 0.U
  fQ.ready := 0.U
  // when getdata switch io.statwSW == cal and filter must all translate to fQ
  when(dodata) {
    fQMuxIn <> io.filter
  }.otherwise{
    fQMuxIn <> fQ
  }
  fQ.ready := state === cal

  val iQMuxIn = Wire(DecoupledIO(io.img.bits.cloneType))
  val iQ = FIFO(iQMuxIn, imgSpadLen)
  io.img.ready := 0.U
  iQ.ready := 0.U
  when(dodata){
    iQMuxIn <> io.img
  }.otherwise{
    iQMuxIn <> iQ
  }
  iQ.ready := state === cal

  val mulReg = RegInit(0.U(w.W))
  val pSumMem = Mem(32, UInt(w.W))
  val calCnt = Counter(32)
  val pSumAddr = Counter(32)

  val trash = RegInit(0.U(w.W))

  io.oSum.valid := 0.U

  switch (state){
    is(idle){
      configReg := io.regConfig
      when(io.stateSW === data){
        state := data
      }
    }
    is(data){
      when(fQMuxIn.fire()){
        fCnt.inc()
      }
      // to swith to cal, filter in must done
      when(io.stateSW === cal){
        state := cal
      }
    }
    is(cal){
      when(fQ.fire() & iQ.fire()){
        when(calCnt.value === fCnt.value - 1.U){
          calCnt.value := 0.U
          pSumAddr.inc()
          pSumMem(pSumAddr.value) := fQ.bits * iQ.bits + pSumMem(pSumAddr.value)
          io.oSum.bits := fQ.bits * iQ.bits + pSumMem(pSumAddr.value)
          io.oSum.valid := 1.U
          state := pDone
        }.otherwise{
          pSumMem(pSumAddr.value) := fQ.bits * iQ.bits + pSumMem(pSumAddr.value)
          io.oSum.bits := 0.U
          calCnt.inc()
        }
      }.otherwise{
        io.oSum.valid := 0.U
      }
    }
    is(pDone){
      fQMuxIn.valid := 0.U
      // let img FIFO spit a data to trash
      iQ.ready := 1.U
      trash := iQ.bits
      core.dontTouch(trash)
      iQMuxIn <> io.img
      when(io.img.valid === 0.U){
        state := allDone
      }.otherwise{
        state := cal
      }
    }
    is(allDone){

    }
  }


}
