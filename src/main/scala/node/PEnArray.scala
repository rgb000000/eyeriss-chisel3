package node

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import chisel3.experimental._
import pe._
import myutil._
import breeze.linalg._
import config._

class PEnArray(implicit p: Parameters) extends Module {
  val dataBits = p(BRAMKey).dataW
  val io = IO(new Bundle {
    val Freader = Flipped(Decoupled(UInt(dataBits.W)))
    val Fid = Input(UInt(p(Shape)._1.W))

    val Ireaders = Vec(p(Shape)._2 + p(Shape)._1 - 1, Flipped(Decoupled(UInt(dataBits.W))))

    val Write =  Vec(p(Shape)._2, DecoupledIO(SInt(p(OSumW).W)))

    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)
    val done = Output(Bool())
    //    val dataDone = Output(Bool())

    val go = Input(Bool())
  })

  val FselectPass = List.fill(p(Shape)._1)(Module(new selectPass(UInt(p(ShellKey).memParams.dataBits.W))))
  FselectPass.foreach(_.io.in <> io.Freader)
  io.Freader.ready := FselectPass.map(_.io.in.ready).reduce(_ | _)
  (FselectPass, io.Fid.toBools).zipped.foreach(_.io.en := _)

  val penarray = Seq.tabulate(p(Shape)._1, p(Shape)._2)((x, y) => {
    Module(new PEn(position = (x, y)))
  })
  io.done := penarray.head.head.io.stateOut === 5.U
  val peLeftandBottom = penarray.toList.map(_.head).toList ::: penarray.toList.last.tail.toList

  require(peLeftandBottom.length == io.Ireaders.length)

  val shiftBits = Seq.tabulate(io.Ireaders.length)((i) => {
    val en = peLeftandBottom(i).io.img.ready
    io.Ireaders(i).ready := en
    ShiftRegister(io.Ireaders(i).bits, p(Shape)._1 + p(Shape)._2 - i, 0.U, en)
  })

  val shiftValid = Seq.tabulate(io.Ireaders.length)((i) => {
    val en = peLeftandBottom(i).io.img.ready
    io.Ireaders(i).ready := en
    ShiftRegister(io.Ireaders(i).valid, p(Shape)._1 + p(Shape)._2 - i, 0.U, en)
  })

  // PEnArray
  // row share filter in
  for (i <- 0 until p(Shape)._1) {
    (penarray.map(_ (i)), FselectPass).zipped.foreach(_.io.filter <> _.io.out)
  }
  // bolique upward image in
  penarray.foreach(_.foreach((PEn) => {
    PEn.io.img.valid := shiftValid(PEn.position._1 + PEn.position._2)
    PEn.io.img.bits := shiftBits(PEn.position._1 + PEn.position._2)
  }))
  // oSum upward
  for (i <- 0 until p(Shape)._1 - 1) {
    (penarray(i), penarray(i + 1)).zipped.foreach(_.io.iSum <> _.io.oSumSRAM)
  }
  penarray.last.foreach((pen) => {
    pen.io.iSum.valid := 0.U
    pen.io.iSum.bits := 0.S
  })
  (io.Write, penarray.head).zipped.foreach(_ <> _.io.oSumSRAM)
  //  val acc = List.fill(p(Shape)._2)(Module(new Acc))
  //  (acc, penarray.head).zipped.foreach(_.io.in <> _.io.oSumSRAM)
  //  (acc, penarray.head).zipped.foreach(_.io.last := _.io.stateOut === 5.U)
  //  acc.foreach(a => {
  //    a.io.bias.valid := io.Breader.valid
  //    a.io.bias.bits := io.Breader.bits(p(BiasW) - 1, 0).asSInt()
  //    io.Breader.ready := a.io.bias.ready
  //  })

  // io assign

  //other config
  val totalSingleFilterNum = WireInit(io.peconfig.singleFilterLen * io.peconfig.nchannel)
  val totalFilterNum = WireInit(totalSingleFilterNum * io.peconfig.filterNum)
  penarray.foreach(_.foreach(_.io.totalSingleFilterNum := totalSingleFilterNum))
  penarray.foreach(_.foreach(_.io.totalFilterNum := totalFilterNum))
  penarray.foreach(_.foreach(_.io.peconfig := io.peconfig))
  penarray.foreach(_.foreach(_.io.stateSW := io.stateSW))

  // write and read and acc
  // TODO: current, only do 1 filterNum

  // dataDone TODO: can only use |_ instead of using all pen dataDone signal
}

class PEnArrayShell(implicit p: Parameters) extends Module {
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val io = IO(new Bundle {
    val FilterBRAM = Flipped(new BRAMInterface(p(BRAMKey).dataW))
    val FilterAddr = Input(UInt(p(BRAMKey).addrW.W))
    val FilterLen = Input(UInt(16.W))

    val ImgBRAM = Flipped(new BRAMInterface(p(BRAMKey).dataW*n))
    val ImgAddr = Input(UInt(p(BRAMKey).addrW.W))
    val ImgLen = Input(UInt(16.W))

//    val writer = new VMEWriteMaster
    val WriteBRAM =  Flipped(new BRAMInterface(p(WriterBRAMW)))

    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)

    val go = Input(Bool())
    val done = Output(Bool())
  })

  val freader = Module(new BRAMFilterReader)
  freader.io.r <> io.FilterBRAM
  freader.io.go := io.go
  freader.io.addr := io.FilterAddr
  freader.io.len := io.FilterLen
  freader.io.totalOutChannel := io.peconfig.totalOutChannel
  val ireader = Module(new BRAMImgReader)
  ireader.io.r <> io.ImgBRAM
  ireader.io.go := io.go
  ireader.io.addr := io.ImgAddr
  ireader.io.len := io.ImgLen

  val accs = List.fill(p(Shape)._2)(Module(new Acc))
  accs.foreach(_.io.accState := io.peconfig.accState)
  accs.foreach(_.io.bias := io.peconfig.bias)
  accs.foreach(_.io.cntLen := io.peconfig.singleImgLen - io.peconfig.singleFilterLen + 1.U)
  val writer = Module(new BRAMWriter)

  val penarray = Module(new PEnArray)
  penarray.io.Freader <> freader.io.dout
  penarray.io.Fid <> freader.io.fid
  penarray.io.Ireaders.foreach(_.valid := ireader.io.doutSplit.valid)
  (penarray.io.Ireaders, ireader.io.doutSplit.bits).zipped.foreach(_.bits := _)
  ireader.io.doutSplit.ready := penarray.io.Ireaders.last.ready
  (accs, penarray.io.Write).zipped.foreach(_.io.in <> _)
  (writer.io.in, accs).zipped.foreach(_ <> _.io.out)
  io.WriteBRAM <> writer.io.w
  io.done := penarray.io.done

  //other config
  val totalSingleFilterNum = WireInit(io.peconfig.singleFilterLen * io.peconfig.nchannel)
  val totalFilterNum = WireInit(totalSingleFilterNum * io.peconfig.filterNum)
  penarray.io.stateSW := io.stateSW
  penarray.io.peconfig := io.peconfig
  penarray.io.go := io.go
}

class PEnArrayShellTestTop(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    //    val writer = new VMEWriteMaster
    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)

    val go = Input(Bool())
    val done = Output(Bool())
  })
  val filterBRAM = Module(new BRAM(p(BRAMKey).dataW, p(FilterMEMPath)))
  val imgBRAM = Module(new BRAM(p(BRAMKey).dataW*(p(Shape)._1 + p(Shape)._2 - 1), p(FeatureMEMPath)))
  val outBRAM = Module(new BRAM(p(WriterBRAMW)))
  val penarray = Module(new PEnArrayShell)
  io.done := penarray.io.done
  penarray.io.FilterBRAM <> filterBRAM.io
  penarray.io.FilterAddr := 0.U
  penarray.io.FilterLen := io.peconfig.singleFilterLen * p(Shape)._1.asUInt() * io.peconfig.filterNum
  penarray.io.ImgBRAM <> imgBRAM.io
  penarray.io.ImgAddr := 0.U
  penarray.io.ImgLen := io.peconfig.singleImgLen
  outBRAM.io <> penarray.io.WriteBRAM
  penarray.io.stateSW := io.stateSW
  penarray.io.peconfig := io.peconfig
  penarray.io.go := io.go
}


object GetVerilogPEnArray extends App {
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_PEnArray_test"), () => new PEnArrayShellTestTop)
}
