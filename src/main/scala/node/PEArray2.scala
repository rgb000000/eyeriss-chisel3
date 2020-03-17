package node

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import chisel3.experimental._
import pe._
import myutil._
import breeze.linalg._
import config._

class PEArray2(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val filter_in = Vec(p(Shape)._1, Flipped(DecoupledIO(SInt(p(FilterW).W))))
    val img_in = Vec(p(Shape)._1 + p(Shape)._2 - 1, Flipped(DecoupledIO(SInt(p(ImgW).W))))
    val bias = Flipped(DecoupledIO(SInt((p(BiasW)).W)))
    val oSum = Vec(p(Shape)._2, DecoupledIO(SInt(p(OSumW).W)))

    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)

    //    val done = Output(UInt(1.W))
    val dataDone = Output(Bool())
  })
  val pearray = Seq.tabulate(p(Shape)._1, p(Shape)._2)((x: Int, y: Int) => {
    Module(new PETesterTop(position = (x, y)))
  })

  // row share filter in
  pearray.foreach((perow: Seq[PETesterTop]) => {
    (perow, io.filter_in).zipped.foreach(_.io.filter <> _)
  })

  // bolique upward image in
  pearray.foreach(_.foreach((pe) => {
    pe.io.img <> io.img_in(pe.position._1 + pe.position._2)
  }))

  //oSum upward
  val addTree = List.fill(p(Shape)._2)(Module(new addTreeDecoupleIO(p(Shape)._1 + 1, 8)))
  for (i <- 0 until p(Shape)._2) {
    (addTree(i).io.seq, pearray.map(_ (i).io.oSumSRAM) :+ io.bias).zipped.foreach((addTreein, depIO) => {
      addTreein.valid := depIO.valid
      addTreein.bits := depIO.bits
      depIO.ready := addTreein.ready
    })
  }
  (io.oSum, addTree).zipped.foreach(_ <> _.io.out)

  //other config
  val totalSingleFilterNum = WireInit(io.peconfig.singleFilterLen * io.peconfig.nchannel)
  val totalFilterNum = WireInit(totalSingleFilterNum * io.peconfig.filterNum)
  pearray.foreach(_.foreach(_.io.totalSingleFilterNum := totalSingleFilterNum))
  pearray.foreach(_.foreach(_.io.totalFilterNum := totalFilterNum))
  pearray.foreach(_.foreach(_.io.peconfig := io.peconfig))
  pearray.foreach(_.foreach(_.io.stateSW := io.stateSW))
  pearray.foreach(_.foreach(_.io.pSumIn.valid := 0.U))
  pearray.foreach(_.foreach(_.io.pSumIn.bits := 0.S))

  // dataDone
  io.dataDone := pearray.map(_.map(_.io.dataDone).reduce(_ | _)).reduce(_ | _)
}

class PEArray2Top(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val Freaders = Vec(p(Shape)._1, new VMEReadMaster)
    val Ireaders = Vec(p(Shape)._1 + p(Shape)._2 - 1, new VMEReadMaster)
    val Breader = new VMEReadMaster
    val writer = new VMEWriteMaster

    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)
    //  val done = Output(UInt(1.W))
    val dataDone = Output(Bool())

    val readLen = Input(UInt(16.W))
    val go = Input(Bool())
  })

  val freader = List.fill(p(Shape)._1)(Module(new Reader))
  val ireader = List.fill(p(Shape)._1 + p(Shape)._2 - 1)(Module(new Reader))
  val breader = Module(new Reader)
  val writer = Module(new Writer)

  val inNums = p(ShellKey).memParams.dataBits / p(FilterW)
  val pearray = List.fill(inNums)(Module(new PEArray2))

  // pearray io assign
  for (i <- 0 until inNums) {
    // filter
    (pearray(i).io.filter_in, freader).zipped.foreach((filterio, reader) => {
      filterio.valid := reader.io.dout.valid
      filterio.bits := reader.io.dout.bits((i + 1) * p(FilterW) - 1, i * p(FilterW)).asSInt()
      reader.io.dout.ready := filterio.ready
    })
    // img
    (pearray(i).io.img_in, ireader).zipped.foreach((imgio, reader) => {
      imgio.valid := reader.io.dout.valid
      imgio.bits := reader.io.dout.bits((i + 1) * p(FilterW) - 1, i * p(FilterW)).asSInt()
      reader.io.dout.ready := imgio.ready
    })
    // bias
    pearray(i).io.bias.valid := breader.io.dout.valid
    pearray(i).io.bias.bits := breader.io.dout.bits((i + 1) * p(FilterW) - 1, i * p(FilterW))
    breader.io.dout.ready := pearray(i).io.bias.ready

  }

  // osum
  for (i <- 0 until p(Shape)._2) {
    writer.io.in(i).valid := pearray.head.io.oSum(i).valid
    for (j <- 0 until inNums) {
      writer.io.in(i).bits(j) := pearray(j).io.oSum(i).bits
    }
    pearray.head.io.oSum(i).ready := writer.io.in(i).ready
  }

  // other
  pearray.foreach(_.io.stateSW := io.stateSW)
  pearray.foreach(_.io.peconfig := io.peconfig)
  io.dataDone := pearray.map(_.io.dataDone).reduce(_ & _)

  // io assign
  (freader, io.Freaders).zipped.foreach((fr, io) => {
    fr.io.read <> io
  })

  (ireader, io.Ireaders).zipped.foreach((ir, io) => {
    ir.io.read <> io
  })
  breader.io.read <> io.Breader

  io.writer <> writer.io.wr

  // readers input assign
  freader.foreach((r) => {
    r.io.length := io.readLen
    r.io.go := io.go
  })
  ireader.foreach((r) => {
    r.io.length := io.readLen
    r.io.go := io.go
  })

}

