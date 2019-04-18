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
    val test = Output(Bool())
    val test1 = Output(SInt(4.W))
    val test2 = Output(SInt(4.W))
  })
  io.test := ((-1).S - io.dataIn.bits.positon.col) === 0.S
  io.test1 := (-1).S
  io.test2 := io.dataIn.bits.positon.col

  val NoC = List[List[Node]]().toBuffer
  for (i <- Range(0, shape._1)) {
    val temp = List[Node]().toBuffer
    for (j <- Range(0, shape._2)) {
      val node = Module(new Node(j == 0, (i, j), w))
      if (j != 0) {
        val pe = Module(new PETesterTop((i, j)))
        pe.io.pSumIn.bits := 0.S
        pe.io.pSumIn.valid := 0.U
        pe.io.oSum.ready := 0.U
        pe.io.stateSW := io.stateSW
        pe.io.peconfig := io.peconfig
        val ds = Module(new dataSwitch())
        ds.io.dataIn <> node.io.dataPackageOut
        pe.io.filter <> ds.io.filter
        pe.io.img <> ds.io.img
      }
      temp.append(node)
    }
    NoC.append(temp.toList)
  }

  // NoC valid and bits
  for (i <- Range(0, shape._1)) {
    for (j <- Range(1, shape._2)) {
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

}
