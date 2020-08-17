package node

import chisel3._
import chisel3.util._
import config._
import pe._
import myutil._

class filterDMA(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val go = Input(Bool())
    val len = Input(UInt(8.W))
    val addr = Input(UInt(8.W))

    val dout = Decoupled(SInt(p(FilterW).W))
  })
  val BRAM = Module(new BRAM(p(FilterW)))

  val idle :: read :: done :: Nil = Enum(3)
  val state = RegInit(idle)
  val len = RegInit(0.U(8.W))
}

class PEArray_arm(implicit p: Parameters) extends Module{
  val col = 10
  val n = 12
  val w = 8

  val io = IO(new Bundle{
    val filterIn = Flipped(Decoupled(Vec(p(Shape)._1, SInt(p(FilterW).W))))
    val featureIn = Flipped(Decoupled(Vec(p(Shape)._1 + p(Shape)._2 -1, SInt(p(ImgW).W))))
    val sumOut = Decoupled(Vec(p(Shape)._2 / 2, SInt(p(OSumW).W)))

    val peconfig = Input(new PEConfigReg)
    val stateSW = Input(UInt(2.W))

    val inB = Vec(col, Flipped(Decoupled(Vec(n, SInt(w.W)))))
    val out = Output(Vec(col, SInt(8.W)))
    val numberIs = Output(UInt(8.W))
  })

  val pearray = Seq.tabulate(p(Shape)._1, p(Shape)._2)((x,y) => {
    Module(new PETesterTop(position = (x, y)))
  })

  // filter In
  for(i  <- 0 until p(Shape)._2){
    (pearray.map(_(i)), io.filterIn.bits).zipped.foreach(_.io.filter.bits <> _)
  }
  pearray.foreach(_.foreach((pe) => {
    pe.io.filter.valid := io.filterIn.valid
  }))
  io.filterIn.ready := pearray.head.head.io.filter.ready

  // feature / image In
  pearray.foreach(_.foreach((pe) => {
    pe.io.img.bits <> io.featureIn.bits(pe.position._1 + pe.position._2)
  }))
  pearray.foreach(_.foreach((pe) => {
    pe.io.img.valid := io.featureIn.valid
  }))
  io.featureIn.ready := pearray.head.head.io.img.ready


  // add trees
  val addTrees = Seq.tabulate(p(Shape)._2)((i) => {
    addTree(
      pearray.map(_(i)).map(_.io.oSumSRAM.bits).toList
    )
  })

  // Max Pooling
  val maxpooling = Module(new maxPooling(24, 2, 5, 8))
  (maxpooling.io.din, addTrees.map((result) => { Mux(result > 0.S, result, 0.S) })).zipped.foreach(_.bits := _ >> 8.U)
  maxpooling.io.din.foreach(_.valid := pearray.head.head.io.oSumSRAM.valid)
  pearray.foreach(_.foreach((pe) => {
    pe.io.oSumSRAM.ready := maxpooling.io.din.head.ready
  }))
  maxpooling.io.peaDone := pearray.head.head.io.stateOut === 5.U
  maxpooling.io.channelOutNum := io.peconfig.filterNum

  // gemmN
  val gemmn = Module(new GEMMn(12, 1, 10, 8))
  (gemmn.io.inA(0).bits, maxpooling.io.dout).zipped.foreach(_ := _.bits)
  gemmn.io.inA(0).valid := maxpooling.io.dout.head.valid
  maxpooling.io.dout.foreach(_.ready := gemmn.io.inA(0).ready)
  gemmn.io.inB <> io.inB
  val tmp = Wire(Vec(col, Vec(n, SInt((w*2).W))))
  gemmn.io.out <> tmp

  val addTrees_2 = Seq.tabulate(col)((i) => {
    addTree(
      tmp(i).toList
    )
  })
  (io.out, addTrees_2).zipped.foreach(_ := _ >> 8.U)

  val ten2one = Module(new Ten2one)
  (ten2one.io.in, io.out).zipped.foreach(_ := _)
  io.numberIs := ten2one.io.out

  // oSum
  (io.sumOut.bits, maxpooling.io.dout).zipped.foreach((osum, maxpool) => osum := maxpool.bits)
  maxpooling.io.dout.foreach((maxpoolOut) =>{
    maxpoolOut.ready := io.sumOut.ready
  })
  io.sumOut.valid := maxpooling.io.dout.head.valid


  val totalSingleFilterNum = WireInit(io.peconfig.singleFilterLen * io.peconfig.nchannel)
  val totalFilterNum = WireInit(totalSingleFilterNum * io.peconfig.filterNum)
  pearray.foreach(_.foreach(_.io.totalSingleFilterNum := totalSingleFilterNum))
  pearray.foreach(_.foreach(_.io.totalFilterNum := totalFilterNum))
  pearray.foreach(_.foreach(_.io.peconfig := io.peconfig))
  pearray.foreach(_.foreach(_.io.stateSW := io.stateSW))
}

object GetVerilogPEArray_ARM extends App {
  implicit val p = new ArmConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_ARM_test"), () => new PEArray_arm)
}
