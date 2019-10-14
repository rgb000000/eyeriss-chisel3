package node

import chisel3._
import chisel3.util._

class Positon(val w: Int) extends Bundle {
  val row = UInt(w.W)
  val col = UInt(1.W)
}

class dataPackage(val w: Int = 8, val n: Int = 35) extends Bundle {
  val data = UInt((8*n).W)
  val dataType = UInt(1.W)
  val positon = new Positon(8)
  val cnt = UInt(8.W)
}

class dataPackageSmall(val w: Int = 8, val n: Int = 1) extends Bundle {
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
    val stop = Output(Bool())
  })

  val x = WireInit(positon._1.asUInt(8.W))
  val y = WireInit(positon._2.asUInt(8.W))
  core.dontTouch(x)
  core.dontTouch(y)

  val qIn = Wire(io.dataPackageIn.cloneType)
  val q = Queue(qIn, 2)

  qIn <> io.dataPackageIn
  q <> io.dataPackageOut

  val boardcast = Wire(Bool())
  io.stop := false.B
  // if row means this node is use to row distrube and doesn't have PE
  if (row) {
    boardcast := 0.U
    io.dataPackageIn.ready := qIn.ready &
      (boardcast |
        ((io.dataPackageIn.bits.positon.row === positon._1.U) & (io.dataPackageIn.bits.dataType === 0.U)) |
        (((io.dataPackageIn.bits.positon.row >= positon._1.U)
          & (io.dataPackageIn.bits.positon.row < positon._1.U + io.colLen.asUInt()))
          & (io.dataPackageIn.bits.dataType === 1.U) & (io.rowLen > positon._1.U))
        )
    qIn.valid := io.dataPackageIn.valid &
      (boardcast |
        ((io.dataPackageIn.bits.positon.row === positon._1.U) & (io.dataPackageIn.bits.dataType === 0.U)) |
        (((io.dataPackageIn.bits.positon.row >= positon._1.U)
          & (io.dataPackageIn.bits.positon.row < positon._1.U + io.colLen.asUInt()))
          & (io.dataPackageIn.bits.dataType === 1.U) & (io.rowLen > positon._1.U))
        )
    when((qIn.ready === 0.U) &
      (boardcast |
        ((io.dataPackageIn.bits.positon.row === positon._1.U) & (io.dataPackageIn.bits.dataType === 0.U)) |
        (((io.dataPackageIn.bits.positon.row >= positon._1.U)
          & (io.dataPackageIn.bits.positon.row < positon._1.U + io.colLen.asUInt()))
          & (io.dataPackageIn.bits.dataType === 1.U) & (io.rowLen > positon._1.U))
        )) {
      io.stop := true.B
    }
  } else {
    boardcast := (io.dataPackageIn.bits.positon.col === 1.U)
    io.dataPackageIn.ready := qIn.ready &
      (boardcast |
//        ((io.dataPackageIn.bits.positon.col === positon._2.U) & (io.dataPackageIn.bits.dataType === 0.U)) |
        ((io.dataPackageIn.bits.positon.row === (positon._2.U(8.W) + positon._1.U - 1.U)) & (io.dataPackageIn.bits.dataType === 1.U))
        )
    qIn.valid := io.dataPackageIn.valid &
      (boardcast |
//        ((io.dataPackageIn.bits.positon.col === positon._2.S) & (io.dataPackageIn.bits.dataType === 0.U)) |
        ((io.dataPackageIn.bits.positon.row === (positon._2.U(8.W) + positon._1.U - 1.U)) & (io.dataPackageIn.bits.dataType === 1.U))
        )
    when((qIn.ready === 0.U) &
      (boardcast |
//        ((io.dataPackageIn.bits.positon.col === positon._2.S) & (io.dataPackageIn.bits.dataType === 0.U)) |
        ((io.dataPackageIn.bits.positon.row === (positon._2.U(8.W) + positon._1.U - 1.U)) & (io.dataPackageIn.bits.dataType === 1.U))
        )
    ){
      io.stop := true.B
    }
  }
//  val test = WireInit(((io.dataPackageIn.bits.positon.row === (positon._2.S(8.W) + positon._1.S - 1.S)) & (io.dataPackageIn.bits.dataType === 1.U)))
//  val whichRow = WireInit(positon._2.S(8.W) + positon._1.S - 1.S)
//  core.dontTouch(test)
//  core.dontTouch(whichRow)

}

// Q2Q 有bug！！ TODO
// 当读取新的一组数据时，此时如果FIFO正好满了，会导致改组数据的第一个数据丢失
// 目前在tb中的解决方法是一次只传输iLen的长度，让其刚好在psum周期读取完
class Q2Q(big: Int = 35, small: Int = 1) extends Module {
  val io = IO(new Bundle {
    val bigIn = Flipped(DecoupledIO(new dataPackage(n = big)))
    val smallOut = DecoupledIO(new dataPackageSmall(8))
  })

  val tmp = Reg(Vec((big / small), UInt(8.W)))
  val cnt_reg = Reg(UInt(8.W))
  val dataType_reg = Reg(UInt(1.W))
  val positon_reg = Reg(new Positon(8).cloneType)
  val cnt = Counter(64)
  val doing = RegInit(false.B)

  val cnt_in = Counter(4096)
  val cnt_out = Counter(4096)
  val cnt_all_in = RegInit(0.U(12.W))
  val cnt_one_in = RegInit(0.U(12.W))
  val cnt_one_in_2 = RegInit(0.U(12.W))
  val zin = WireInit(cnt_in.value)
  val zout = WireInit(cnt_out.value)

  val QIn = Wire(io.smallOut.cloneType)
  val Q = Queue(QIn, 35)

  core.dontTouch(zin)
  core.dontTouch(zout)
  core.dontTouch(cnt_all_in)
  core.dontTouch(cnt_one_in)
  core.dontTouch(cnt_one_in_2)

  when((doing === true.B) & QIn.fire()) {
    cnt_one_in := cnt_one_in + 1.U
  }
  when((doing === false.B) & QIn.fire()) {
    cnt_one_in := 1.U
    cnt_one_in_2 := cnt_one_in
  }

  when(io.bigIn.fire() & io.bigIn.bits.dataType === 1.U) {
    cnt_all_in := cnt_all_in + io.bigIn.bits.cnt
  }
  when(QIn.fire() & QIn.bits.dataType === 1.U) {
    cnt_in.inc()
  }
  when(Q.fire() & Q.bits.dataType === 1.U) {
    cnt_out.inc()
  }


  QIn.valid := 0.U
  QIn.bits.data := 0.S
  QIn.bits.positon.row := 0.U
  QIn.bits.positon.col := 0.U
  QIn.bits.dataType := 0.U

  io.smallOut <> Q

  io.bigIn.ready := doing === false.B

  when(io.bigIn.fire()) {
    when(io.bigIn.bits.cnt === 1.U) {
      doing := false.B
      cnt.value := 0.U
    }.otherwise {
      doing := true.B
      cnt.value := 1.U
    }
    for (i <- tmp.indices){
      tmp(i) := io.bigIn.bits.data((i+1)*8-1, i*8).asUInt()
    }
    cnt_reg := io.bigIn.bits.cnt
    positon_reg := io.bigIn.bits.positon
    dataType_reg := io.bigIn.bits.dataType

    QIn.bits.data := io.bigIn.bits.data(7,0).asSInt()
    QIn.bits.positon := io.bigIn.bits.positon
    QIn.bits.dataType := io.bigIn.bits.dataType
    QIn.valid := 1.U
  }

  when(doing === true.B) {
    when((cnt.value === cnt_reg - 1.U) & QIn.fire()) {
      doing := false.B
      cnt.value := 0.U
    }.elsewhen(QIn.fire()) {
      cnt.inc()
    }.otherwise {

    }
    QIn.bits.data := tmp(cnt.value).asSInt()
    QIn.bits.positon := positon_reg
    QIn.bits.dataType := dataType_reg
    QIn.valid := 1.U
  }

}
