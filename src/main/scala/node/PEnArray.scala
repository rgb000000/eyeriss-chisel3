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

    val Write = DecoupledIO(SInt(p(OSumW).W))

    val stateSW = Input(UInt(2.W))
    val peconfig = Input(new PEConfigReg)
    //  val done = Output(UInt(1.W))
    //    val dataDone = Output(Bool())

    val go = Input(Bool())
  })

  val FselectPass = List.fill(p(Shape)._1)(Module(new selectPass(UInt(p(ShellKey).memParams.dataBits.W))))
  FselectPass.foreach(_.io.in <> io.Freader)
  (FselectPass, io.Fid.toBools).zipped.foreach(_.io.en := _)

  val penarray = Seq.tabulate(p(Shape)._1, p(Shape)._2)((x, y) => {
    Module(new PEn(position = (x, y)))
  })
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
  val arb = Module(new Arbiter(SInt(p(OSumW).W), p(Shape)._2))
  (arb.io.in, penarray.head).zipped.foreach(_ <> _.io.oSumSRAM)
  io.Write <> arb.io.out
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

  val penarray = Seq.tabulate(p(Shape)._1, p(Shape)._2)((x, y) => {
    Module(new PEn(position = (x, y)))
  })

  // PEnArray
  // row share filter in
  penarray.foreach(penRow => {
    (penRow, freader).zipped.foreach((PEn, fReader) => {
      PEn.io.filter <> fReader.io.dout
    })
  })
  // bolique upward image in
  penarray.foreach(_.foreach((PEn) => {
    PEn.io.img <> ireader(PEn.position._1 + PEn.position._2).io.dout
  }))
  // oSum upward
  val acc = List.fill(p(Shape)._2)(Module(new Acc))
  //  //change to pulse array
  //  val addTree = List.fill(p(Shape)._2)(Module(new addTreeDecoupleIO(p(Shape)._1, p(OSumW))))
  //  for (i <- 0 until p(Shape)._2) {
  //    (addTree(i).io.seq, penarray.map(_ (i).io.oSumSRAM)).zipped.foreach((addTreein, depIO) => {
  //      addTreein.valid := depIO.valid
  //      addTreein.bits := depIO.bits
  //      depIO.ready := addTreein.ready
  //    })
  //  }
  //  (acc, addTree).zipped.foreach(_.io.in <> _.io.out)
  for (i <- 0 until p(Shape)._1 - 1) {
    (penarray(i), penarray(i + 1)).zipped.foreach(_.io.iSum <> _.io.oSumSRAM)
  }
  penarray.last.foreach((pen) => {
    pen.io.iSum.valid := 0.U
    pen.io.iSum.bits := 0.U
  })
  (acc, penarray.head).zipped.foreach(_.io.in <> _.io.oSumSRAM)
  (acc, penarray.head).zipped.foreach(_.io.last := _.io.stateOut === 5.U)
  acc.foreach(a => {
    a.io.bias.valid := breader.io.dout.valid
    a.io.bias.bits := breader.io.dout.bits(p(BiasW) - 1, 0).asSInt()
    breader.io.dout.ready := a.io.bias.ready
  })

  (writer.io.in, acc).zipped.foreach(_ <> _.io.out)

  // io assign
  (freader, io.Freaders).zipped.foreach(_.io.read <> _)
  (ireader, io.Ireaders).zipped.foreach(_.io.read <> _)
  breader.io.read <> io.Breader
  writer.io.wr <> io.writer


  //other config
  val totalSingleFilterNum = WireInit(io.peconfig.singleFilterLen * io.peconfig.nchannel)
  val totalFilterNum = WireInit(totalSingleFilterNum * io.peconfig.filterNum)
  penarray.foreach(_.foreach(_.io.totalSingleFilterNum := totalSingleFilterNum))
  penarray.foreach(_.foreach(_.io.totalFilterNum := totalFilterNum))
  penarray.foreach(_.foreach(_.io.peconfig := io.peconfig))
  penarray.foreach(_.foreach(_.io.stateSW := io.stateSW))

  // write and read and acc
  // TODO: current, only do 1 filterNum
  freader.foreach(_.io.length := totalSingleFilterNum)
  freader.foreach(_.io.go := io.go)
  ireader.foreach(_.io.length := totalSingleFilterNum)
  ireader.foreach(_.io.go := io.go)
  breader.io.go := io.go
  breader.io.length := 1.U

  // dataDone TODO: can only use |_ instead of using all pen dataDone signal
  io.dataDone := penarray.map(_.map(_.io.dataDone).reduce(_ & _)).reduce(_ & _)
}

object GetVerilogPEnArray extends App {
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_PEnArray_test"), () => new PEnArray)
}
