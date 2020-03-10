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
    val peconfig = Input(new PEConfigReg(8))

//    val done = Output(UInt(1.W))
    val dataDone = Output(Bool())
  })
  val pearray = Seq.tabulate(p(Shape)._1, p(Shape)._2)((x: Int, y: Int) => {
    Module(new PETesterTop(position = (x, y)))
  })

  // row share filter in
  pearray.foreach((perow: Seq[PETesterTop])=>{
    (perow, io.filter_in).zipped.foreach(_.io.filter <> _)
  })

  // bolique upward image in
  pearray.foreach(_.foreach((pe) => {
    pe.io.img <> io.img_in(pe.position._1 + pe.position._2)
  }))

  //oSum upward
  val addTree = List.fill(p(Shape)._2)(Module(new addTreeDecoupleIO(p(Shape)._1 + 1, 8)))
  for(i <- 0 until p(Shape)._2) {
    (addTree(i).io.seq, pearray.map(_(i).io.oSumSRAM) :+ io.bias).zipped.foreach((addTreein, depIO)=>{
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

object Test extends App {
  val a = Seq.tabulate(3, 3)(_*3 + _)
  println(a(0))
  val c = Seq(1,2,3)
  println(c :+ 4)
  val d = Seq(1,2,3,4,5,6,7)
  def groupAndAdd(seq:Seq[Int]): Seq[Int] ={
    seq.grouped(2).toList.map(_ match {
      case a: Seq[Int] if a.length == 1 => {a(0)}
      case a: Seq[Int] if a.length >1 => {a(0) + a(1)}
    })
  }
  var e = groupAndAdd(d)
  while(e.length != 1){
    e = groupAndAdd(e)
  }
//  val b = DenseMatrix.tabulate(3, 3)((x: Int, y: Int) => {
//    x
//  })
//  print(b)
}
