package ram

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._

class URAMInterface(implicit p: Parameters) extends Bundle {
  val we = Input(Bool())
  val raddr = Input(UInt(p(URAMKey).addrW.W))
  val waddr = Input(UInt(p(URAMKey).addrW.W))
  val din = Input(UInt(p(AccW).W))
  val dout = Output(UInt(p(AccW).W))
}

class URAM(aw: Int, dw: Int) extends BlackBox(Map("aw" -> aw, "dw" -> dw))
  with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val we = Input(Bool())
    val waddr = Input(UInt(aw.W))
    val din = Input(UInt(dw.W))
    val raddr = Input(UInt(aw.W))
    val dout = Output(UInt(dw.W))
  })
  setResource("/ultraRAM.v")
}

class Avalue2MaxChannel(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    /*
    *** w channel ***
    [[op]]:     read(0) or write(1)
    [[waddr]]:  w addr
    [[wdata]]:  w data
    */
    val op = Input(Bool())
    val waddr = Input(UInt((p(URAMKey).addrW + log2Ceil(p(MaxChannel))).W))
    val wdata = Input(UInt(p(AccW).W))
    /*
    *** r channel ***
    [[raddr]]:  r addr
    [[rdata]]:  r data
    */
    val rdataMaxChannel = Decoupled(UInt((p(MaxChannel) * p(AccW)).W))
  })

  val urams = Seq.fill(p(MaxChannel) - 1)(Module(new URAM(p(URAMKey).addrW, p(AccW))))

  // write channel
  val wIdx = WireInit((io.waddr >> p(URAMKey).addrW).asUInt())
  urams.zipWithIndex.foreach({
    case (uram: URAM, n: Int) => {
      uram.io.clk := clock
      when(wIdx === n.asUInt()) {
        uram.io.we := io.op
        uram.io.waddr := io.waddr
        uram.io.din := io.wdata
      }.otherwise{
        uram.io.we := 0.U
        uram.io.waddr := 511.U
        uram.io.din := 0.U
      }
    }
  })

  val canRead = RegNext(wIdx === (p(MaxChannel) - 1).asUInt() & (io.op === 1.U))
  val raddr = Counter(1 << p(URAMKey).addrW)
  val qIn = Wire(DecoupledIO(UInt((p(AccW) * p(MaxChannel)).W)))
  val q = Queue(qIn, 2)
  io.rdataMaxChannel <> q

  // read channel
  val doutChannelParallel = Wire(Vec(p(MaxChannel), UInt(p(AccW).W)))
  (doutChannelParallel, urams :+ io.wdata).zipped.collect({
    case (dout, uram: URAM) => {
      uram.io.raddr := raddr.value
      dout := uram.io.dout
    }
    case (dout, wdata: UInt) => {
      dout := RegNext(wdata)
    }
    case (dout, _) => {
      println("T is error")
      assert(false)
    }
  })
  when(wIdx === (p(MaxChannel) - 1).asUInt() & (io.op === 1.U)){
    raddr.inc()
  }
  qIn.valid := canRead
  qIn.bits := doutChannelParallel.asUInt()
}

class ResultRecover(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val dataChannelParallel = Flipped(DecoupledIO(UInt((p(AccW) * p(MaxChannel)).W)))
    val raddr = Input(UInt(p(URAMKey).addrW.W))
    val rdata = Output(UInt((p(AccW) * p(MaxChannel)).W))
  })

  val uram = Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel)))
  uram.io.clk := clock

  io.dataChannelParallel.ready := 1.U
  val addr = Counter(4096)
  when(io.dataChannelParallel.fire()){
    uram.io.we := 1.U
    uram.io.waddr := addr.value
    uram.io.din := io.dataChannelParallel.bits
    addr.inc()
  }.otherwise{
    uram.io.we := 0.U
  }

  uram.io.raddr := io.raddr
  io.rdata := uram.io.dout
}

class Reorder(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    // left
    val op = Input(Bool())
    val waddr = Input(UInt((p(URAMKey).addrW + log2Ceil(p(MaxChannel))).W))
    val wdata = Input(UInt(p(AccW).W))
    // right
    val raddr = Input(UInt(p(URAMKey).addrW.W))
    val rdata = Output(UInt((p(AccW) * p(MaxChannel)).W))
  })
  val widthCvt = Module(new Avalue2MaxChannel)
  val recover = Module(new ResultRecover)

  widthCvt.io.rdataMaxChannel <> recover.io.dataChannelParallel

  widthCvt.io.op := io.op
  widthCvt.io.waddr := io.waddr
  widthCvt.io.wdata := io.wdata

  recover.io.raddr := io.raddr
  io.rdata := recover.io.rdata
}

object GetVeilogAvalue2Channel extends App{
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_uram_verilog"), () => new Avalue2MaxChannel)
}
