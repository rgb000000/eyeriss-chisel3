package node

import chisel3._
import chisel3.util._

class Positon(val w: Int) extends Bundle {
  val row = SInt(w.W)
  val col = SInt(w.W)
}

class dataPackage(val w: Int) extends Bundle {
  val data = SInt(w.W)
  val dataType = UInt(1.W)
  val positon = new Positon(8)
}

class Node(row: Boolean, positon: (Int, Int), w: Int) extends Module {
  val io = IO(new Bundle {
    val dataPackageIn = Flipped(DecoupledIO(new dataPackage(w)))
    val dataPackageOut = DecoupledIO(new dataPackage(w))
    val colLen = Input(UInt(w.W))
    val rowLen = Input(UInt(w.W))
  })

  val x = WireInit(positon._1.asUInt(8.W))
  val y = WireInit(positon._2.asUInt(8.W))
  core.dontTouch(x)
  core.dontTouch(y)

  val qIn = Wire(io.dataPackageIn.cloneType)
  val q = Queue(qIn, 5)

  qIn <> io.dataPackageIn
  q <> io.dataPackageOut

  val boardcast = Wire(Bool())

  // if row means this node is use to row distrube and doesn't have PE
  if (row) {
    boardcast := (io.dataPackageIn.bits.positon.row === (-1).S)
    io.dataPackageIn.ready := qIn.ready &
      (boardcast |
        ((io.dataPackageIn.bits.positon.row === positon._1.S) & (io.dataPackageIn.bits.dataType === 0.U)) |
        (((io.dataPackageIn.bits.positon.row >= positon._1.S)
          & (io.dataPackageIn.bits.positon.row < positon._1.S + io.colLen.asSInt()))
          & (io.dataPackageIn.bits.dataType === 1.U) & (io.rowLen > positon._1.U))
        )
    qIn.valid := io.dataPackageIn.valid &
      (boardcast |
        ((io.dataPackageIn.bits.positon.row === positon._1.S) & (io.dataPackageIn.bits.dataType === 0.U)) |
        (((io.dataPackageIn.bits.positon.row >= positon._1.S)
          & (io.dataPackageIn.bits.positon.row < positon._1.S + io.colLen.asSInt()))
          & (io.dataPackageIn.bits.dataType === 1.U) & (io.rowLen > positon._1.U))
        )
  } else {
    boardcast := (io.dataPackageIn.bits.positon.col === (-1).S)
    io.dataPackageIn.ready := qIn.ready &
      (boardcast |
        ((io.dataPackageIn.bits.positon.col === positon._2.S) & (io.dataPackageIn.bits.dataType === 0.U)) |
        ((io.dataPackageIn.bits.positon.row === (positon._2.S(8.W) + positon._1.S - 1.S)) & (io.dataPackageIn.bits.dataType === 1.U))
        )
    qIn.valid := io.dataPackageIn.valid &
      (boardcast |
        ((io.dataPackageIn.bits.positon.col === positon._2.S) & (io.dataPackageIn.bits.dataType === 0.U)) |
        ((io.dataPackageIn.bits.positon.row === (positon._2.S(8.W) + positon._1.S - 1.S)) & (io.dataPackageIn.bits.dataType === 1.U))
        )
  }
  val test = WireInit(((io.dataPackageIn.bits.positon.row === (positon._2.S(8.W) + positon._1.S - 1.S)) & (io.dataPackageIn.bits.dataType === 1.U)))
  val whichRow = WireInit(positon._2.S(8.W) + positon._1.S - 1.S)
  core.dontTouch(test)
  core.dontTouch(whichRow)

}
