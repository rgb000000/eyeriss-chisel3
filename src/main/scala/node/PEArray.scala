package node

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import pe._

@chiselName
class dataSwitch(w: Int = 16) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new dataPackage(w)))
    val filter = DecoupledIO(SInt(w.W))
    val img = DecoupledIO(SInt(w.W))
    //    val pSum = DecoupledIO(SInt(w.W))
  })
  io.filter.bits := 0.S
  io.filter.valid := 0.U
  io.img.bits := 0.S
  io.img.valid := 0.U
  io.dataIn.ready := 0.U
  when(io.dataIn.bits.dataType === 0.U) {
    io.filter.bits := io.dataIn.bits.data
    io.filter.valid := io.dataIn.valid
    io.dataIn.ready := io.filter.ready
  }.otherwise {
    io.img.bits := io.dataIn.bits.data
    io.img.valid := io.dataIn.valid
    io.dataIn.ready := io.img.ready
  }
}

class PEArray(shape: (Int, Int), w: Int = 16) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new dataPackage(w).cloneType))
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg(16))
//    val oSumMEM = Vec(shape._2, DecoupledIO(dataIn.bits.data.cloneType))
    val oSumSRAM = Vec(shape._2, DecoupledIO(dataIn.bits.data.cloneType))
  })

  val NoC = List[List[Node]]().toBuffer
  val pes = List[List[PETesterTop]]().toBuffer
  for (i <- Range(0, shape._1)) {
    val tempNoC = List[Node]().toBuffer
    val tempPE = List[PETesterTop]().toBuffer
    for (j <- Range(0, shape._2 + 1)) {
      val node = Module(new Node(j == 0, (i, j), w))
      if (j != 0) {
        val pe = Module(new PETesterTop((i, j - 1)))
        pe.io.pSumIn.bits := 0.S
        pe.io.pSumIn.valid := 0.U
        pe.io.oSumMEM.ready := 0.U
        pe.io.oSumSRAM.ready := 0.U
        pe.io.stateSW := io.stateSW
        pe.io.peconfig := io.peconfig
        val ds = Module(new dataSwitch())
        ds.io.dataIn <> node.io.dataPackageOut
        pe.io.filter <> ds.io.filter
        pe.io.img <> ds.io.img
        tempPE.append(pe)
      }
      tempNoC.append(node)
    }
    NoC.append(tempNoC.toList)
    pes.append(tempPE.toList)
  }

  // NoC valid and bits
  for (i <- Range(0, shape._1)) {
    for (j <- Range(1, shape._2 + 1)) {
      NoC(i)(j).io.dataPackageIn.valid := NoC(i).head.io.dataPackageOut.valid
      NoC(i)(j).io.dataPackageIn.bits := NoC(i).head.io.dataPackageOut.bits
    }
  }

  // NoC ready
  for (i <- NoC) {
    i.head.io.dataPackageOut.ready := i.tail.map(_.io.dataPackageIn.ready).reduce(_ | _)
    i.head.io.dataPackageIn.valid := io.dataIn.valid
    i.head.io.dataPackageIn.bits := io.dataIn.bits
  }
  io.dataIn.ready := NoC.map(_.head.io.dataPackageIn.ready).reduce(_ | _)

//  require(io.oSumMEM.length == shape._2)
//  require(pes.length == shape._1)
//  require(pes.head.length == shape._2)
//  val forTest = Wire(Vec(io.oSumMEM.length, Vec(shape._1, io.oSumMEM.head.bits.cloneType)))
//  forTest.foreach(_.foreach(core.dontTouch(_)))
//  for(i <- io.oSumMEM.indices){
//    var j = 0
//    pes.map(_(i)).map(_.io.oSumMEM.bits).foreach((x) => {forTest(i)(j) := x; j += 1})
//  }
//  for(i <- io.oSumMEM.indices){
//    io.oSumMEM(i).valid := pes.map(_(i)).map(_.io.oSumMEM.valid).reduce(_ | _)
//    io.oSumMEM(i).bits := pes.map(_(i)).map((x)=>{
//      val tmp = Wire(SInt(w.W))
//      when(x.io.oSumMEM.valid === 1.U){
//        tmp := x.io.oSumMEM.bits
//      }.otherwise{
//        tmp := 0.S
//      }
//      tmp
//    }).reduce(_ + _)
//    pes.map(_(i)).foreach(_.io.oSumMEM.ready := io.oSumMEM(i).valid & io.oSumMEM(i).ready)
//  }

  require(io.oSumSRAM.length == shape._2)
  require(pes.length == shape._1)
  require(pes.head.length == shape._2)
  for(i <- io.oSumSRAM.indices){
    io.oSumSRAM(i).valid := pes.map(_(i)).map(_.io.oSumSRAM.valid).reduce(_ | _)
    io.oSumSRAM(i).bits := pes.map(_(i)).map((x)=>{
      val tmp = Wire(SInt(w.W))
      when(x.io.oSumSRAM.valid === 1.U){
        tmp := x.io.oSumSRAM.bits
      }.otherwise{
        tmp := 0.S
      }
      tmp
    }).reduce(_ + _)
    pes.map(_(i)).foreach(_.io.oSumSRAM.ready := io.oSumSRAM(i).valid & io.oSumSRAM(i).ready)
  }

}
