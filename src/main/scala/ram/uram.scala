package ram

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._

class URAMInterface(implicit p: Parameters) extends Bundle {
  val we = Input(Bool())
  val raddr = Input(UInt(p(URAMKey).addrW.W))
  val waddr = Input(UInt(p(URAMKey).addrW.W))
  val din = Input(UInt(p(URAMKey).dataW.W))
  val dout = Output(UInt(p(URAMKey).dataW.W))
}

class URAM(implicit p: Parameters) extends BlackBox(Map("aw" -> p(URAMKey).addrW, "dw" -> p(URAMKey).dataW))
  with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val we = Input(Bool())
    val waddr = Input(UInt(p(URAMKey).addrW.W))
    val din = Input(UInt(p(URAMKey).dataW.W))
    val raddr = Input(UInt(p(URAMKey).addrW.W))
    val dout = Output(UInt(p(URAMKey).dataW.W))
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
    val raddr = Input(UInt(p(URAMKey).addrW.W))
    val rdata = Output(UInt((p(MaxChannel) * p(URAMKey).dataW).W))
  })

  val urams = Seq.fill(p(MaxChannel))(Module(new URAM))

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

  // read channel
  val doutChannelParallel = Wire(Vec(p(MaxChannel), UInt(p(AccW).W)))
  (doutChannelParallel, urams).zipped.foreach({
    case (dout, uram) => {
      uram.io.raddr := io.raddr
      dout := uram.io.dout
    }
  })
  io.rdata := doutChannelParallel.asUInt()
}

object GetVeilogAvalue2Channel extends App{
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_uram_verilog"), () => new Avalue2MaxChannel)
}
