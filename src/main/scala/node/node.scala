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
  val cnt = UInt(5.W)
}

class dataPackageSmall(val w: Int) extends Bundle {
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

class Q2Q(big: Int = 280, small: Int = 8) extends Module {
  val io = IO(new Bundle {
    val bigIn = Flipped(DecoupledIO(new dataPackage(big)))
    val smallOut = DecoupledIO(new dataPackageSmall(small))
  })

  val tmp = Reg(Vec(big / small, new dataPackage(small).data.cloneType))
  val cnt_reg = Reg(UInt(5.W))
  val dataType_reg = Reg(UInt(1.W))
  val positon_reg = Reg(new Positon(8).cloneType)
  val cnt = Counter(64)
  val doing = RegInit(false.B)

  val QIn = Wire(io.smallOut.cloneType)
  val Q = Queue(QIn, 40)

  QIn.valid := 0.U
  QIn.bits.data := 0.S
  QIn.bits.positon.row := 0.S
  QIn.bits.positon.col := 0.S
  QIn.bits.dataType := 0.U

  io.smallOut <> Q

  io.bigIn.ready := doing === false.B

  when(io.bigIn.fire()) {
    doing := true.B
    cnt.value := 1.U
    for (i <- tmp.indices) {
      tmp(i) := io.bigIn.bits.data((i + 1) * small - 1, i * small).asSInt()
    }
    cnt_reg := io.bigIn.bits.cnt
    positon_reg := io.bigIn.bits.positon
    dataType_reg := io.bigIn.bits.dataType

    QIn.bits.data := io.bigIn.bits.data(small - 1, 0).asSInt()
    QIn.bits.positon := io.bigIn.bits.positon
    QIn.bits.dataType := io.bigIn.bits.dataType
    QIn.valid := 1.U
  }

  when(doing === true.B){
    when((cnt.value === cnt_reg - 1.U) & QIn.fire()){
      doing := false.B
      cnt.value := 0.U
    }.elsewhen(QIn.fire()){
      cnt.inc()
    }.otherwise{

    }
    QIn.bits.data := tmp(cnt.value)
    QIn.bits.positon := io.bigIn.bits.positon
    QIn.bits.dataType := io.bigIn.bits.dataType
    QIn.valid := 1.U
  }

}
