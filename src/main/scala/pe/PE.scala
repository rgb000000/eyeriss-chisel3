package pe

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import myutil._



class PEConfigReg(val w:Int = 16) extends Bundle{
  val filterNum = UInt(w.W)
  val singleFilterLen = UInt(w.W)
  val imgNum = UInt(w.W)
  val singleImgLen = UInt(w.W)
  val nchannel = UInt(w.W)
}


// take care!  in PE stateSw is buffer one time
@chiselName
class PE(filterSpadLen: Int = 225, imgSpadLen: Int = 225, pSumMemLen: Int = 256, w:Int = 16) extends Module{
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
  val iCnt = Counter(256)
  val calCnt = Counter(256)
  val fCalCnt = Counter(256)
  val cCalCnt = Counter(256)
  val pDoneCnt = Counter(256)
  val pSumAddr = Counter(256)
  val zfc = WireInit(fCnt.value)
  val zic = WireInit(iCnt.value)
  val zcc = WireInit(calCnt.value)
  val zfcc = WireInit(fCalCnt.value)
  val zccc = WireInit(cCalCnt.value)
  val zpc = WireInit(pDoneCnt.value)
  val zpsa = WireInit(pSumAddr.value)
  core.dontTouch(zfc)
  core.dontTouch(zic)
  core.dontTouch(zcc)
  core.dontTouch(zfcc)
  core.dontTouch(zccc)
  core.dontTouch(zpc)
  core.dontTouch(zpsa)
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
  val iQreg = Reg(iQ.bits.cloneType)
  io.img.ready := 0.U
  iQ.ready := 0.U
  when(dodata){
    iQMuxIn <> io.img
    when(iCnt.value === configReg.singleFilterLen){
      iQMuxIn.valid := 0.U
      io.img.ready := 0.U
    }
  }.otherwise{
    iQMuxIn <> iQ
  }
  iQ.ready := (state === cal) & (fCalCnt.value === 0.U)

  val mulReg = RegInit(0.U(w.W))
  val pSumMem = Mem(pSumMemLen, UInt(w.W))

  val trash = RegInit(0.U(w.W))

  io.oSum.valid := 0.U

  val normal :: multiFilter :: multiChannel :: Nil = Enum(3)
  val mode = Wire(UInt(8.W))
  when(configReg.nchannel =/= 1.U){
    mode := multiChannel
  }.elsewhen(configReg.filterNum =/= 1.U){
    mode := multiFilter
  }.otherwise{
    mode := normal
  }

  val addr = Wire(UInt(64.W))
  val singleResultLen = configReg.singleImgLen - configReg.singleFilterLen + 1.U
  addr := 0.U
  when(mode === normal){
    addr := pSumAddr.value
  }.elsewhen(mode === multiFilter){
    addr := /*pSumAddr.value + */  fCalCnt.value // * singleResultLen
  }.elsewhen(mode === multiChannel){
    addr := pSumAddr.value
  }
  val pSumReg = Reg(UInt(w.W))



  switch (state){
    is(idle){
      configReg := io.regConfig
      when(io.stateSW === data){
        state := data
      }
    }
    is(data){
      switch(mode){
        is(normal){
          when(fQMuxIn.fire()){
            fCnt.inc()
          }
          // to swith to cal, filter in must done
          when(io.stateSW === cal){
            state := cal
          }
        }
        is(multiFilter){
          when(fQMuxIn.fire()){
            fCnt.inc()
          }
          when(iQMuxIn.fire()){
            iCnt.inc()
          }
          when(io.stateSW === cal){
            state := cal
          }
        }
        is(multiChannel){
          when(fQMuxIn.fire()){
            fCnt.inc()
          }
          when(io.stateSW === cal){
            state := cal
          }
        }
      }
    }
    is(cal){
      switch(mode){
        is(normal){
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

        is(multiFilter){
          iQMuxIn.valid := 0.U
          val memdata = pSumMem(addr)

          io.oSum.bits := 0.U
          when(   (calCnt.value === configReg.singleFilterLen)) {
            io.oSum.valid := 1.U
            io.oSum.bits := fQ.bits * iQreg + pSumMem(addr)
                                      /*^*/
          }.elsewhen((calCnt.value ===/*|*/configReg.singleFilterLen - 1.U) & (fCalCnt.value === 0.U)){ // 0 mean the last
            io.oSum.valid := 1.U      /*v*/   // when the first result it use iQ.bits, else will use iQreg
            io.oSum.bits := fQ.bits * iQ.bits + pSumMem(addr)
          }

          when(fQ.fire() & iQ.fire()){
            iQMuxIn.valid := 1.U
            pSumMem(addr) := fQ.bits * iQ.bits + pSumMem(addr)
            iQreg := iQ.bits
            fCalCnt.inc()
            calCnt.inc()
          }.elsewhen(fQ.fire()){
            pSumMem(addr) := fQ.bits * iQreg + pSumMem(addr)
            when(fCalCnt.value === configReg.filterNum - 1.U){
              fCalCnt.value := 0.U
            }.otherwise{
              fCalCnt.inc()
            }

//            calCnt.inc()
            when((calCnt.value === configReg.singleFilterLen) & (fCalCnt.value === configReg.filterNum - 1.U)){
              calCnt.value := 0.U
              pSumAddr.inc()
              state := pDone
            }
          }
        }

        is(multiChannel){
          when(fQ.fire() & iQ.fire()){
            when(calCnt.value === fCnt.value - 1.U){
              calCnt.value := 0.U
              pSumAddr.inc()
              pSumMem(addr) := fQ.bits * iQ.bits + pSumMem(addr)
              io.oSum.bits := fQ.bits * iQ.bits + pSumMem(addr)
              io.oSum.valid := 1.U
              when(io.img.valid){
                state := pDone
              }.otherwise{
                state := allDone
              }
            }.otherwise{
              pSumMem(addr) := fQ.bits * iQ.bits + pSumMem(addr)
              io.oSum.bits := 0.U
              calCnt.inc()
            }
          }.otherwise{
            io.oSum.valid := 0.U
          }
        }
      }
    }
    is(pDone){
      for(i <- Range(0, pSumMemLen)){
        pSumMem(i) := 0.U
      }
      switch(mode){
        is(normal){
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
        is(multiFilter){
          fQMuxIn.valid := 0.U
          // let img FIFO spit a data to trash
          iQ.ready := 1.U
          trash := iQ.bits
          core.dontTouch(trash)
          iQMuxIn <> io.img
          when((io.img.valid === 0.U) | (pSumAddr.value === configReg.singleImgLen - iCnt.value + 1.U)){
            state := allDone
          }.otherwise{
            state := cal
          }
        }
        is(multiChannel){
          fQMuxIn.valid := 0.U
          // let img FIFO spit a data to trash
          iQ.ready := 1.U
          trash := iQ.bits
          core.dontTouch(trash)
          iQMuxIn <> io.img
          pDoneCnt.inc()
          when(pDoneCnt.value === configReg.nchannel - 1.U){
            state := cal
            pDoneCnt.value := 0.U
          }.otherwise{
            state := pDone
          }
//          when(io.img.valid === 0.U){
//            state := allDone
//          }.otherwise{
//            state := cal
//          }
        }
      }
    }
    is(allDone){

    }
  }


}
