package node

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import chisel3.experimental._
import pe._
import myutil._

@chiselName
class dataSwitch(w: Int = 8) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new dataPackageSmall(w)))
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

class PEArray(val shape: (Int, Int), w: Int = 8) extends Module {
  val io = IO(new Bundle {
    val dataIn = Flipped(DecoupledIO(new dataPackage(8).cloneType))
    val bias = Flipped(DecoupledIO(SInt((w*2).W)))
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg(8))
    //    val oSumMEM = Vec(shape._2, DecoupledIO(dataIn.bits.data.cloneType))
    val oSumSRAM = Vec(shape._2, DecoupledIO(SInt(8.W)))
    val done = Output(UInt(1.W))
    val dataDone = Output(Bool())
  })


  val doneReg = RegInit(0.U(1.W))
  val doneRegReg = RegNext(doneReg)
  io.done := RegNext(doneRegReg)       // wait pooling done

  val dataInQ = Queue(io.dataIn, 2)
  val colLen = WireInit(io.peconfig.singleImgLen - io.peconfig.singleFilterLen + 1.U)
  val rowLen = WireInit(io.peconfig.singleFilterLen)

  val totalFilterNum = RegNext(io.peconfig.singleFilterLen * io.peconfig.nchannel)

  val NoC = List[List[Node]]().toBuffer
  val pes = List[List[PETesterTop]]().toBuffer
  for (i <- Range(0, shape._1)) {
    val tempNoC = List[Node]().toBuffer
    val tempPE = List[PETesterTop]().toBuffer
    for (j <- Range(0, shape._2 + 1)) {
      val node = Module(new Node(j == 0, (i, j), w))
      if (j != 0) {
        val pe = Module(new PETesterTop((i, j - 1), w))
        pe.io.totalFilterNum := totalFilterNum
        pe.io.pSumIn.bits := 0.S
        pe.io.pSumIn.valid := 0.U
        //        pe.io.oSumMEM.ready := 0.U
        pe.io.oSumSRAM.ready := 0.U
        pe.io.stateSW := io.stateSW
        pe.io.peconfig := io.peconfig
        val ds = Module(new dataSwitch())
        val q2q = Module(new Q2Q())
        q2q.io.bigIn <> node.io.dataPackageOut
        q2q.io.smallOut <> ds.io.dataIn
        pe.io.filter <> ds.io.filter
        pe.io.img <> ds.io.img
        tempPE.append(pe)
      }
      node.io.colLen := colLen
      node.io.rowLen := rowLen
      tempNoC.append(node)
    }
    NoC.append(tempNoC.toList)
    pes.append(tempPE.toList)
  }

  val stop = WireInit(NoC.map(_.head.io.stop).reduce(_ | _))

  val handleStop = Module(new handleStop())
  handleStop.io.dataIn <> dataInQ

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
    i.head.io.dataPackageIn.valid := handleStop.io.dataOut.valid
    i.head.io.dataPackageIn.bits := handleStop.io.dataOut.bits
  }
  handleStop.io.dataOut.ready := NoC.map(_.head.io.dataPackageIn.ready).reduce(_ | _)
  handleStop.io.stop := stop

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

  val biasqIn = Wire(DecoupledIO(SInt((w*2).W)))
  val biasQ = Queue(biasqIn, 8)
  val dobias = Wire(UInt(1.W))
  dobias := 0.U
  dontTouch(dobias)
  biasQ.ready := 0.U
  io.bias.ready := 0.U
  when(io.stateSW === 1.U){
    biasqIn <> io.bias
  }.otherwise{
    biasqIn.bits := biasQ.bits
    dobias := pes.map(_ (0)).map(_.io.oSumSRAM.valid).reduce(_ | _)
    biasQ.ready := dobias
    biasqIn.valid := dobias
  }

  require(io.oSumSRAM.length == shape._2)
  require(pes.length == shape._1)
  require(pes.head.length == shape._2)
  for (i <- io.oSumSRAM.indices) {
    io.oSumSRAM(i).valid := RegNext(pes.map(_ (i)).map(_.io.oSumSRAM.valid).reduce(_ | _))
    val saturation = Module(new Saturation())
    val temp = Wire(SInt(16.W))
    temp := RegNext(pes.map(_ (i)).map((x) => {
      val tmp = Wire(SInt((16).W))
      when(x.io.oSumSRAM.valid === 1.U) {
        tmp := x.io.oSumSRAM.bits
      }.otherwise {
        tmp := 0.S
      }
      tmp
    }).reduce(_ + _) + biasQ.bits)
    saturation.io.dataIn := temp
    when(io.peconfig.relu === 1.U & saturation.io.dataOut < 0.S) {
      io.oSumSRAM(i).bits := 0.S
    }.otherwise {
      io.oSumSRAM(i).bits := saturation.io.dataOut
    }
    pes.map(_ (i)).foreach(_.io.oSumSRAM.ready := io.oSumSRAM(i).ready)
  }

  val idle :: data :: cal :: pDone :: newImg :: allDone :: Nil = Enum(6)
  doneReg := pes.map(_.map(_.io.stateOut === allDone)).flatten.reduce(_ & _)
  io.dataDone := pes.map(_.map(_.io.dataDone).reduce(_ & _)).reduce(_ & _)

}
