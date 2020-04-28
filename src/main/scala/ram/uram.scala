package ram

import chisel3._
import chisel3.util._
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
  addResource("/ultraRAM.v")
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

    val length = Input(UInt(p(URAMKey).addrW.W))
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
      }.otherwise {
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
  when(wIdx === (p(MaxChannel) - 1).asUInt() & (io.op === 1.U)) {
    when(raddr.value === io.length - 1.U){
      raddr.value := 0.U
    }.otherwise{
      raddr.inc()
    }
  }
  qIn.valid := canRead
  qIn.bits := doutChannelParallel.asUInt()
}



class One2MaxChannel(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val din = Flipped(Decoupled(SInt(p(OSumW).W)))
    val rowLength = Input(UInt(p(URAMKey).addrW.W))
    val dout = Decoupled(UInt((p(OSumW) * p(MaxChannel)).W))
    val done = Output(Bool())
  })

  // addr generate
  val idx = Counter(p(MaxChannel))
  val addr = Counter(512)
  val waddr = (idx.value << p(URAMKey).addrW.U).asUInt() + addr.value
  val we = WireInit(false.B)
  val wdata = WireInit(0.asUInt(p(AccW).W))
  val one2maxc = Module(new Avalue2MaxChannel)
  one2maxc.io.length := io.rowLength
  // wire <> one2mac
  one2maxc.io.op := we
  one2maxc.io.waddr := waddr
  one2maxc.io.wdata := wdata
  one2maxc.io.rdataMaxChannel <> io.dout
  // addr logic
  io.din.ready := true.B
  io.done := false.B
  when(io.din.fire()){
    we := true.B
    wdata := io.din.bits.asUInt()
    when(addr.value === io.rowLength - 1.U){
      idx.inc()
      addr.value := 0.U
      io.done := true.B
    }.otherwise{
      addr.inc()
    }
  }
}

class MaxChannelReorder (implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val dins = Vec(p(Shape)._2, Flipped(Decoupled(UInt((p(OSumW) * p(MaxChannel)).W))))
    val length = Input(UInt(p(URAMKey).addrW.W))
    val forceOut = Input(Bool())

    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
  })
  val ping :: pong :: last :: Nil = Enum(3)
  val state = RegInit(ping)
  val lastState = RegInit(ping)
  val canOut = RegInit(false.B)

  val qs = Seq.tabulate(p(Shape)._2)(i => Queue(io.dins(i), 16))
  val uramPP = Seq.fill(2, p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  uramPP.foreach(_.foreach(_.io.clk := clock))
  val waddr = Counter(512) // max length of a row

  val outAddr = Counter(512)

  switch(state){
    is(ping){
      when((waddr.value === io.length - 1.U) & qs.head.fire()){
        when(io.forceOut){
          state := last
          lastState := state
        }.otherwise{
          state := pong
        }
      }
    }
    is(pong){
      when((waddr.value === io.length - 1.U) & qs.head.fire()){
        when(io.forceOut){
          state := last
          lastState := state
        }.otherwise{
          state := ping
        }
      }
      canOut := true.B
    }
    is(last){
      // because have 1 cycle lantency to read uram so ,there is not (len - 1)
      when(outAddr.value === io.length){
        state := ping
      }
    }
  }

  uramPP(0).foreach(_.io.waddr := waddr.value)
  uramPP(0).foreach(_.io.we := false.B)
  (uramPP(0), qs.map(_.bits)).zipped.foreach(_.io.din := _)
  uramPP(1).foreach(_.io.waddr := waddr.value)
  uramPP(1).foreach(_.io.we := false.B)
  (uramPP(1), qs.map(_.bits)).zipped.foreach(_.io.din := _)

  when(qs.map(_.valid).reduce(_ & _)){
    qs.foreach(_.ready := true.B)
  }.otherwise{
    qs.foreach(_.ready := false.B)
  }
  when(qs.head.fire()){
    when(state === ping){
      when(waddr.value === io.length - 1.U){
        waddr.value := 0.U
      }.otherwise{
        waddr.inc()
      }
      uramPP(0).foreach(_.io.we := true.B)
    }.elsewhen(state === pong){
      when(waddr.value === io.length - 1.U){
        waddr.value := 0.U
      }.otherwise{
        waddr.inc()
      }
      uramPP(1).foreach(_.io.we := true.B)
    }
  }

  io.outValid := false.B
  io.inputRowDataOut.foreach(_ := 0.U)
  io.outAddr := outAddr.value
  val valid = RegInit(false.B)
  uramPP.foreach(_.foreach(_.io.raddr := outAddr.value))

  when(canOut){
    when(qs.head.fire()){
      when(waddr.value === io.length - 1.U){
        outAddr.value := 0.U
      }.otherwise{
        outAddr.inc()
        valid := true.B
      }
      io.outValid := true.B
      when(state === pong){
        (io.inputRowDataOut, uramPP(0).map(_.io.dout) :+ qs(0).bits :+ qs(1).bits).zipped.foreach(
          _ := _
        )
      }.elsewhen(state === ping){
        (io.inputRowDataOut, uramPP(1).map(_.io.dout) :+ qs(0).bits :+ qs(1).bits).zipped.foreach(
          _ := _
        )
      }
    }
  }

  when(state === last){
    when(outAddr.value === io.length){
      outAddr.value := 0.U
    }.otherwise{
      outAddr.inc()
    }
    io.outValid := outAddr.value =/= 0.U
    when(lastState === ping){
      (io.inputRowDataOut, uramPP(0).map(_.io.dout) :+ 0.U :+ 0.U).zipped.foreach(_ := _)
    }.elsewhen(lastState === pong){
      (io.inputRowDataOut, uramPP(1).map(_.io.dout) :+ 0.U :+ 0.U).zipped.foreach(_ := _)
    }
  }
}

class NewWB(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    // left
    val dins = Vec(p(Shape)._2, Flipped(Decoupled(SInt(p(OSumW).W))))
    val rowLength = Input(UInt(p(URAMKey).addrW.W))
    val forceOut = Input(Bool())
    // right
    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))

    val done = Output(Bool())
  })
  val one2Maxcs = Seq.fill(p(Shape)._2)(Module(new One2MaxChannel))
  (one2Maxcs, io.dins).zipped.foreach(_.io.din <> _)
  one2Maxcs.foreach(_.io.rowLength := io.rowLength)
  val reorder = Module(new MaxChannelReorder)
  reorder.io.forceOut := io.forceOut
  (reorder.io.dins, one2Maxcs).zipped.foreach(_ <> _.io.dout)
  reorder.io.length := io.rowLength
  io.inputRowDataOut := reorder.io.inputRowDataOut
  io.outValid := reorder.io.outValid
  io.outAddr := reorder.io.outAddr
  io.done := one2Maxcs.head.io.done
}



object GetVeilogAvalue2Channel extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_uram_verilog"), () => new Avalue2MaxChannel)
}

object GetVeilogNewWB extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_newwb_verilog"), () => new NewWB)
}
