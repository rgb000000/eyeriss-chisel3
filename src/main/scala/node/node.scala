package node

import chisel3._
import chisel3.util._

class Positon(val w: Int) extends Bundle {
  val row = SInt(w.W)
  val col = SInt(w.W)
}

class dataPackage(val w: Int = 8, val n:Int = 35) extends Bundle {
  val data = Vec(n, SInt(w.W))
  val dataType = UInt(1.W)
  val positon = new Positon(8)
  val cnt = UInt(8.W)
}

class dataPackageSmall(val w: Int = 8, val n:Int = 1) extends Bundle {
  val data = Vec(n, SInt(w.W))
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

class Q2Q(big: Int = 35, small: Int = 1) extends Module {
  val io = IO(new Bundle {
    val bigIn = Flipped(DecoupledIO(new dataPackage(n = big)))
    val smallOut = DecoupledIO(new dataPackageSmall(8))
  })

  val tmp = Reg(Vec((big / small), new dataPackageSmall(n = small).data(0).cloneType))
  val cnt_reg = Reg(UInt(8.W))
  val dataType_reg = Reg(UInt(1.W))
  val positon_reg = Reg(new Positon(8).cloneType)
  val cnt = Counter(64)
  val doing = RegInit(false.B)

  val QIn = Wire(io.smallOut.cloneType)
  val Q = Queue(QIn, 40)

  QIn.valid := 0.U
  QIn.bits.data(0) := 0.S
  QIn.bits.positon.row := 0.S
  QIn.bits.positon.col := 0.S
  QIn.bits.dataType := 0.U

  io.smallOut <> Q

  io.bigIn.ready := doing === false.B

  when(io.bigIn.fire()) {
    when(io.bigIn.bits.cnt === 1.U){
      doing := false.B
      cnt.value := 0.U
    }.otherwise{
      doing := true.B
      cnt.value := 1.U
    }
    (tmp, io.bigIn.bits.data).zipped.foreach(_ := _)
    cnt_reg := io.bigIn.bits.cnt
    positon_reg := io.bigIn.bits.positon
    dataType_reg := io.bigIn.bits.dataType

    QIn.bits.data(0) := io.bigIn.bits.data(0)
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
    QIn.bits.data(0) := tmp(cnt.value)
    QIn.bits.positon := positon_reg
    QIn.bits.dataType := dataType_reg
    QIn.valid := 1.U
  }

}
