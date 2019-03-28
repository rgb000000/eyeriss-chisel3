// implement conv1d

package conv

import chisel3._

class Conv1d(val filterLen: Int, val imgLen: Int, w: Int) extends Module {
  val io = IO(new Bundle {
    val filterRow = Input(Vec(filterLen, SInt(w.W)))
    val imgRow = Input(Vec(imgLen, SInt(w.W)))

    val sumIn = Input(Vec(imgLen - filterLen + 1, SInt(w.W)))

    val filterRowOut = Output(Vec(filterLen, SInt(w.W)))
    val sum = Output(Vec(imgLen - filterLen + 1, SInt(w.W)))
  })

  val localConvSum = Wire(Vec(imgLen - filterLen + 1, Vec(filterLen, SInt(w.W))))
  val tempSum = Wire(Vec(imgLen - filterLen + 1, SInt(w.W)))

  for (i <- 0 until imgLen - filterLen + 1) {
    for (j <- 0 until filterLen) {
      localConvSum(i)(j) := io.filterRow(j) * io.imgRow(j + i)
    }
    tempSum(i) := localConvSum(i).reduce(_ + _)
  }

  for (i <- 0 until imgLen - filterLen + 1) {
    io.sum(i) := tempSum(i) + io.sumIn(i)
  }
  io.filterRowOut := io.filterRow
}


class PEArray(
               val filterRowNum: Int,
               val filterLen: Int,
               val imgRowNum: Int,
               val imgLen: Int,
               val w: Int) extends Module {
  val io = IO({
    new Bundle {
      val filterIn = Input(Vec(filterRowNum, Vec(filterLen, SInt(w.W))))
      val imgIn = Input(Vec(imgRowNum, Vec(imgLen, SInt(w.W))))

      val sum = Output(Vec(imgRowNum - filterRowNum + 1, Vec(imgLen - filterLen + 1,  SInt(w.W))))
    }
  })
  val PEs = Array.fill(filterRowNum, imgRowNum - filterRowNum + 1)(Module(new Conv1d(filterLen, imgLen, w)).io)

  // connect output sum
  //  (io.sum, PEs(0)).zipped.map((iosum, PE)=>{(iosum, PE.sum).zipped.map(_ := _)})
  (io.sum, PEs(0)).zipped.map(_ := _.sum)
  // connect input filter
  (PEs.map(_.head), io.filterIn).zipped.map(_.filterRow := _)
  // connect every PE sum
  for (i <- 0 until filterRowNum - 1) {
    (PEs(i), PEs(i + 1)).zipped.map(_.sumIn := _.sum)
  }
  (PEs(filterRowNum - 1), Array.fill(imgRowNum - filterRowNum + 1, imgLen - filterLen + 1)(0.S)).zipped.map(_.sumIn := _)

  for (i <- 0 until filterRowNum) {
    for(j <- 1 until imgRowNum - filterRowNum + 1){
      PEs(i)(j).filterRow := PEs(i)(j-1).filterRowOut
    }
  }

  // connect imgIn
  for (i <- 0 until filterRowNum) {
    for (j <- 0 until imgRowNum - filterRowNum + 1) {
      (PEs(i)(j).imgRow, io.imgIn(i + j)).zipped.map(_ := _)
    }
  }
}


