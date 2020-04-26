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
    raddr.inc()
  }
  qIn.valid := canRead
  qIn.bits := doutChannelParallel.asUInt()
}

class ResultRecover(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val dataChannelParallel = Flipped(DecoupledIO(UInt((p(AccW) * p(MaxChannel)).W)))
    val raddr = Input(UInt(p(URAMKey).addrW.W))
    val rdata = Output(UInt((p(AccW) * p(MaxChannel)).W))
  })

  val uram = Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel)))
  uram.io.clk := clock

  io.dataChannelParallel.ready := 1.U
  val addr = Counter(4096)
  when(io.dataChannelParallel.fire()) {
    uram.io.we := 1.U
    uram.io.waddr := addr.value
    uram.io.din := io.dataChannelParallel.bits
    addr.inc()
  }.otherwise {
    uram.io.we := 0.U
  }

  uram.io.raddr := io.raddr
  io.rdata := uram.io.dout
}

class Reorder(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // left
    val op = Input(Bool())    // we
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

class One2MaxChannel(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val din = Flipped(Decoupled(SInt(p(OSumW).W)))
    val rowLength = Input(UInt(p(URAMKey).addrW.W))
    val dout = Decoupled(UInt((p(OSumW) * p(MaxChannel)).W))
  })

  // addr generate
  val idx = Counter(p(MaxChannel))
  val addr = Counter(512)
  val waddr = (idx.value << p(URAMKey).addrW.U).asUInt() + addr.value
  val we = WireInit(false.B)
  val wdata = WireInit(0.asUInt(p(AccW).W))
  val one2maxc = Module(new Avalue2MaxChannel)
  // wire <> one2mac
  one2maxc.io.op := we
  one2maxc.io.waddr := waddr
  one2maxc.io.wdata := wdata
  one2maxc.io.rdataMaxChannel <> io.dout
  // addr logic
  io.din.ready := true.B
  when(io.din.fire()){
    we := true.B
    wdata := io.din.bits.asUInt()
    when(addr.value === io.rowLength - 1.U){
      idx.inc()
      addr.value := 0.U
    }.otherwise{
      addr.inc()
    }
  }
}

class MaxChannelReorder (implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val dins = Vec(p(Shape)._2, Flipped(Decoupled(UInt((p(OSumW) * p(MaxChannel)).W))))
    val length = Input(UInt(p(URAMKey).addrW.W))

    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
  })
  val ping :: pong :: Nil = Enum(2)
  val state = RegInit(ping)
  val canOut = RegInit(false.B)

  val qs = Seq.tabulate(p(Shape)._2)(i => Queue(io.dins(i), 16))
  val uramPP = Seq.fill(2, p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  uramPP.foreach(_.foreach(_.io.clk := clock))
  val waddr = Counter(512) // max length of a row


  switch(state){
    is(ping){
      when(waddr.value === io.length - 1.U){
        state := pong
      }
    }
    is(pong){
      when(waddr.value === io.length - 1.U){
        state := ping
      }
      canOut := true.B
    }
  }

  uramPP(0).foreach(_.io.waddr := waddr.value)
  uramPP(0).foreach(_.io.we := false.B)
  (uramPP(0), io.dins.map(_.bits)).zipped.foreach(_.io.din := _)
  uramPP(1).foreach(_.io.waddr := waddr.value)
  uramPP(1).foreach(_.io.we := false.B)
  (uramPP(1), io.dins.map(_.bits)).zipped.foreach(_.io.din := _)

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

  val outAddr = Counter(512)
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
}

class NewWB(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    // left
    val dins = Vec(p(Shape)._2, Flipped(Decoupled(SInt(p(OSumW).W))))
    val rowLength = Input(UInt(p(URAMKey).addrW.W))
    // right
    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
  })
  val one2Maxcs = Seq.fill(p(Shape)._2)(Module(new One2MaxChannel))
  (one2Maxcs, io.dins).zipped.foreach(_.io.din <> _)
  one2Maxcs.foreach(_.io.rowLength := io.rowLength)
  val reorder = Module(new MaxChannelReorder)
  (reorder.io.dins, one2Maxcs).zipped.foreach(_ <> _.io.dout)
  reorder.io.length := io.rowLength
  io.inputRowDataOut := reorder.io.inputRowDataOut
  io.outValid := reorder.io.outValid
  io.outAddr := reorder.io.outAddr
}

class RowDataRecover(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val go = Input(Bool())
    val raddr = Output(UInt(p(URAMKey).addrW.W))
    val rdata = Input(Vec(p(Shape)._2, UInt((p(AccW) * p(MaxChannel)).W)))

    val length = Input(UInt(p(URAMKey).addrW.W))

    val out = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
    val outValid = Output(Bool())
  })

  val idle :: ping :: pong :: Nil = Enum(3)
  val state = RegInit(idle)

  val len = Reg(UInt(p(URAMKey).addrW.W))
  val canOut = RegInit(false.B)

  val valid = RegInit(false.B)
  val raddr = Counter(512) // max length of a row
  val waddr = Counter(512) // max length of a row
  val uramPP = Seq.fill(2, p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  uramPP.foreach(_.foreach(_.io.clk := clock))

  val outAddr = Counter(512)

  // state switch
  switch(state) {
    is(idle) {
      when(io.go) {
        state := ping
      }
    }
    is(ping) {
      when(waddr.value === len - 1.U){
        state := pong
      }
    }
    is(pong){
      canOut := true.B
      when(waddr.value === len - 1.U){
        state := ping
      }
    }
  }

  // idle
  when(state === idle){
    len := io.length
  }
  // ping and pong
  io.raddr := raddr.value
  when(state === ping | state === pong) {
    raddr.inc()
    valid := true.B
  }

  uramPP(0).foreach(_.io.waddr := waddr.value)
  uramPP(0).foreach(_.io.we := false.B)
  (uramPP(0), io.rdata).zipped.foreach(_.io.din := _)
  uramPP(1).foreach(_.io.waddr := waddr.value)
  uramPP(1).foreach(_.io.we := false.B)
  (uramPP(1), io.rdata).zipped.foreach(_.io.din := _)
  when(valid === true.B) {
    when(state === ping){
      when(waddr.value === len - 1.U){
        waddr.value := 0.U
      }.otherwise{
        waddr.inc()
      }
      uramPP(0).foreach(_.io.we := true.B)
    }.elsewhen(state === pong){
      when(waddr.value === len - 1.U){
        waddr.value := 0.U
      }.otherwise{
        waddr.inc()
      }
      uramPP(1).foreach(_.io.we := true.B)
    }
  }

  // out data logic
  io.outValid := false.B
  io.out.foreach(_ := 0.U)
  io.outAddr := outAddr.value
  val valid2 = Reg(Bool())
  valid2 := false.B
  uramPP.foreach(_.foreach(_.io.raddr := outAddr.value))
  when(canOut){
    when(valid){
      when(waddr.value === len - 1.U){
        outAddr.value := 0.U
      }.otherwise{
        outAddr.inc()
        valid2 := true.B
      }
      io.outValid := true.B
      when(state === pong){
        (io.out, uramPP(0).map(_.io.dout) :+ io.rdata(0) :+ io.rdata(1)).zipped.foreach(
          _ := _
        )
      }.elsewhen(state === ping){
        (io.out, uramPP(1).map(_.io.dout) :+ io.rdata(0) :+ io.rdata(1)).zipped.foreach(
          _ := _
        )
      }
    }
  }
}

class WB (implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    // left
    val fromCol = Vec(p(Shape)._2, Flipped(Decoupled(SInt(p(OSumW).W))))
    val length = Input(UInt(9.W))

    //right
    val out = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
    val outValid = Output(Bool())
  })

  val reorders = Seq.fill(p(Shape)._2)(Module(new Reorder))
  val reorderAddr = Seq.fill(p(Shape)._2)(Counter(512))
  io.fromCol.foreach(_.ready := 1.U)
  (reorders, io.fromCol).zipped.foreach(_.io.wdata := _.bits.asUInt())
  (reorders, reorderAddr).zipped.foreach(_.io.waddr := _.value)
  for(i <- 0 until(p(Shape)._2)){
    when(io.fromCol(i).fire()){
      reorderAddr(i).inc()
      reorders(i).io.op := true.B
    }
  }

  val rowDataRcv = Module(new RowDataRecover)
  reorders.foreach(_.io.raddr := rowDataRcv.io.raddr)
  (rowDataRcv.io.rdata, reorders).zipped.foreach(_ := _.io.rdata)
  rowDataRcv.io.length := io.length
  io.out := rowDataRcv.io.out
  io.outAddr := rowDataRcv.io.outAddr
  io.outValid := rowDataRcv.io.outValid
}

object GetVeilogAvalue2Channel extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_uram_verilog"), () => new Avalue2MaxChannel)
}

object GetVeilogNewWB extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_newwb_verilog"), () => new NewWB)
}
