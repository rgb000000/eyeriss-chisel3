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
    when(raddr.value === io.length - 1.U) {
      raddr.value := 0.U
    }.otherwise {
      raddr.inc()
    }
  }
  qIn.valid := canRead
  qIn.bits := doutChannelParallel.asUInt()
}


class One2MaxChannel(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
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
  when(io.din.fire()) {
    we := true.B
    wdata := io.din.bits.asUInt()
    when(addr.value === io.rowLength - 1.U) {
      idx.inc()
      addr.value := 0.U
      io.done := true.B
    }.otherwise {
      addr.inc()
    }
  }
}

class MaxPoolingOut(implicit p: Parameters) extends Module {
  val n = p(Shape)._2 + p(FilterSize) - 1
  val io = IO(new Bundle {
    val inRowDataOut = Input(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val inValid = Input(Bool())
    val inAddr = Input(UInt(p(URAMKey).addrW.W))

    val RowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))

    val length = Input(UInt(p(URAMKey).addrW.W))
    val forceOut = Input(Bool())
    val last = Output(Bool())
  })

  io.outAddr := 0.U
  io.RowDataOut.foreach(_ := 0.U)
  io.outValid := 0.U
  io.last := 0.U

  val RowData = Wire(Vec(p(Shape)._2, UInt((p(AccW) * p(MaxChannel)).W)))
  val valid = Wire(Bool())
  RowData.foreach(_ := 0.U)
  valid := false.B

  val lastRowValid = io.length % p(Shape)._2.U + p(Shape)._2.U

  val din = List.fill(n)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
  for (i <- 0 until n) {
    for (j <- 0 until p(MaxChannel)) {
      din(i)(j) := io.inRowDataOut(i)((j + 1) * p(AccW) - 1, j * p(AccW)).asSInt()
    }
  }
  val buf = List.fill(n)(RegInit(VecInit(Seq.fill(p(MaxChannel))(0.asSInt(p(AccW).W)))))
  val getData :: inRowPool :: getData2 :: inRowPool2andRowPool :: lastGetData1 :: lastGetData2 :: Nil = Enum(6)
  val state = RegInit(getData)
  val uramWaddr = Counter(256)
  val uramRaddr = Counter(256)
  val uramPP = Seq.fill(p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  val afterPoolWaddr = Counter(256)
  uramPP.foreach(_.io.clk := clock)
  uramPP.foreach(_.io.din := 0.U)
  uramPP.foreach(_.io.waddr := uramWaddr.value)
  uramPP.foreach(_.io.we := 0.U)
  uramPP.foreach(_.io.raddr := uramRaddr.value)
  val afterPoolRaddr = Counter(256)
  val afterPoolPP = Seq.fill(p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  val afterPoolPP2 = Seq.fill(p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  val lastRow = Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel)))
  val cnt = Counter(512)
  afterPoolPP.foreach(_.io.clk := clock)
  afterPoolPP.foreach(_.io.din := 0.U)
  afterPoolPP.foreach(_.io.waddr := uramRaddr.value)
  afterPoolPP.foreach(_.io.we := 0.U)
  afterPoolPP.foreach(_.io.raddr := afterPoolRaddr.value)

  afterPoolPP2.foreach(_.io.clk := clock)
  afterPoolPP2.foreach(_.io.din := 0.U)
  afterPoolPP2.foreach(_.io.waddr := uramRaddr.value)
  afterPoolPP2.foreach(_.io.we := 0.U)
  afterPoolPP2.foreach(_.io.raddr := afterPoolRaddr.value)

  val canOut = RegInit(false.B)
  val poolCnt = Counter(2)

  def max(a: SInt, b: SInt): SInt = {
    Mux(a > b, a, b)
  }
  when(io.inValid) {
    when(cnt.value === io.length - 1.U) {
      cnt.value := 0.U
    }.otherwise {
      cnt.inc()
    }
  }

  switch(state) {
    is(getData) {
      when(io.forceOut) {
        state := lastGetData1
      }.elsewhen(io.inValid) {
        state := inRowPool
      }
    }
    is(inRowPool) {
      when(io.inValid & (cnt.value === io.length - 1.U)) {
        state := getData2
      }.elsewhen(io.inValid) {
        state := getData
      }
    }
    is(getData2) {
      when(io.forceOut){
        state := lastGetData1
      }.elsewhen(io.inValid) {
        state := inRowPool2andRowPool
      }
    }
    is(inRowPool2andRowPool) {
      when(io.inValid & (cnt.value === io.length - 1.U)) {
        state := getData
      }.elsewhen(io.inValid) {
        state := getData2
      }
    }
    is(lastGetData1) {
      when(io.inValid){
        state := lastGetData2
      }
    }
    is(lastGetData2){
      when(io.inValid){
        state := lastGetData1
      }
    }
  }
  when(((state === getData) | (state === getData2)) & io.inValid) {
    for (i <- 0 until n) {
      for (j <- 0 until p(MaxChannel)) {
        buf(i)(j) := din(i)(j)
      }
    }
  }

  when((state === inRowPool) & io.inValid) {
    //inRow
    val temp = List.fill(n)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until n) {
      for (j <- 0 until p(MaxChannel)) {
        temp(i)(j) := max(buf(i)(j), din(i)(j))
      }
    }
    (uramPP.map(_.io.din), temp.map(_.asUInt())).zipped.foreach(_ := _)
    uramPP.foreach(_.io.we := 1.U)
    when(uramWaddr.value === (io.length >> 1.U).asUInt() - 1.U) {
      uramWaddr.value := 0.U
    }.otherwise {
      uramWaddr.inc()
    }
  }

  when((state === inRowPool2andRowPool) & io.inValid) {
    // in Row
    val temp = List.fill(n)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until n) {
      for (j <- 0 until p(MaxChannel)) {
        temp(i)(j) := max(buf(i)(j), din(i)(j))
      }
    }
    // between Row
    val uramDout = List.fill(p(Shape)._2)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until p(Shape)._2) {
      for (j <- 0 until p(MaxChannel)) {
        uramDout(i)(j) := uramPP(i).io.dout((j + 1) * p(AccW) - 1, j * p(AccW)).asSInt()
      }
    }
    val topn = List.fill(p(Shape)._2)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for(i <- 0 until p(Shape)._2){
      (topn(i), temp(i)).zipped.foreach(_ := _)
    }
    val last2 = Wire(Vec(p(MaxChannel), SInt(p(AccW).W)))
    for (i <- 0 until p(MaxChannel)){
      last2(i) := max(temp(n-2)(i), temp(n-1)(i))
    }
    val forPool = (uramDout ::: topn).grouped(2).toList
    val temp2 = List.fill(p(Shape)._2)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until p(Shape)._2) {
      for (j <- 0 until p(MaxChannel)) {
        temp2(i)(j) := max(forPool(i)(0)(j), forPool(i)(1)(j))
      }
    }
    (RowData, temp2.map(_.asUInt())).zipped.foreach(_ := _)
    valid := true.B
    when(uramRaddr.value === (io.length >> 1.U).asUInt() - 1.U) {
      uramRaddr.value := 0.U
      canOut := true.B
      poolCnt.inc()
    }.otherwise {
      uramRaddr.inc()
    }
  }

  var rowNum = p(Shape)._1 + p(Shape)._2 - 1
  if(rowNum % 2 == 1){
    rowNum -= 1
  }
  val lastBuf = List.fill(rowNum/2)(RegInit(VecInit(Seq.fill(p(MaxChannel))(0.asSInt(p(AccW).W)))))
  when((state === lastGetData1) & io.inValid){
    val temp = List.fill(rowNum)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until rowNum) {
      for (j <- 0 until p(MaxChannel)) {
        temp(i)(j) := din(i)(j)
      }
    }
    val temp2 = temp.grouped(2).toList
    for (i <- 0 until rowNum/2){
      for(j <- 0 until p(MaxChannel)){
        lastBuf(i)(j) := max(temp2(i)(0)(j), temp2(i)(1)(j))
      }
    }
  }
  when((state === lastGetData2) & io.inValid){
    val temp = List.fill(rowNum)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until rowNum) {
      for (j <- 0 until p(MaxChannel)) {
        temp(i)(j) := din(i)(j)
      }
    }
    val temp2 = temp.grouped(2).toList
    val temp3 = List.fill(rowNum/2)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until rowNum/2){
      for(j <- 0 until p(MaxChannel)){
        temp3(i)(j) := max(temp2(i)(0)(j), temp2(i)(1)(j))
      }
    }
    val temp4 = List.fill(rowNum/2)(Wire(Vec(p(MaxChannel), SInt(p(AccW).W))))
    for (i <- 0 until rowNum/2){
      for(j <- 0 until p(MaxChannel)){
        temp4(i)(j) := max(temp3(i)(j), lastBuf(i)(j))
      }
    }
    for(i <- 0 until rowNum/2){
      RowData(i) := temp4(i).asUInt()
    }
    valid := true.B
  }
  val reorder = Module(new MaxChannelReorder)
  (reorder.io.dins, RowData).zipped.foreach(_.bits := _)
  reorder.io.dins.foreach(_.valid := valid)
  reorder.io.length := (io.length >> 1.U).asUInt()
  reorder.io.forceOut := io.forceOut

  io.RowDataOut <> reorder.io.inputRowDataOut
  io.outValid := reorder.io.outValid

}

class MaxChannelReorder(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val dins = Vec(p(Shape)._2, Flipped(Decoupled(UInt((p(OSumW) * p(MaxChannel)).W))))
    val length = Input(UInt(p(URAMKey).addrW.W))
    val forceOut = Input(Bool())

    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))

    val last = Output(Bool())
  })
  val ping :: pong :: last :: Nil = Enum(3)
  val state = RegInit(ping)
  val lastState = RegInit(ping)
  val canOut = RegInit(false.B)
  val rowCnt = Counter(512)
  val rowCntValue = WireInit(rowCnt.value)
  dontTouch(rowCntValue)

  val lastRowValid = io.length % p(Shape)._2.U

  val step = (p(Shape)._1 + p(Shape)._2 - 3).U
  val remainder = (step - ((io.length - (p(Shape)._1 + p(Shape)._2 - 1).U) % step)) % step

  val qs = Seq.tabulate(p(Shape)._2)(i => Queue(io.dins(i), 16))
  val uramPP = Seq.fill(2, p(Shape)._2)(Module(new URAM(p(URAMKey).addrW, p(AccW) * p(MaxChannel))))
  uramPP.foreach(_.foreach(_.io.clk := clock))
  val waddr = Counter(512) // max length of a row

  val outAddr = Counter(512)

  io.last := false.B

  switch(state) {
    is(ping) {
      when((waddr.value === io.length - 1.U) & qs.head.fire()) {
        rowCnt.inc()
        when(io.forceOut) {
          state := last
          lastState := state
        }.otherwise {
          state := pong
        }
      }
    }
    is(pong) {
      when((waddr.value === io.length - 1.U) & qs.head.fire()) {
        rowCnt.inc()
        when(io.forceOut) {
          state := last
          lastState := state
        }.otherwise {
          state := ping
        }
      }
      canOut := true.B
    }
    is(last) {
      when(lastRowValid =/= 0.U){
        state === ping
      }.otherwise{
        // because have 1 cycle lantency to read uram so ,there is not (len - 1)
        io.last := 1.U
        when(outAddr.value === io.length) {
          state := ping
        }
      }
    }
  }

  uramPP(0).foreach(_.io.waddr := waddr.value)
  uramPP(0).foreach(_.io.we := false.B)
  (uramPP(0), qs.map(_.bits)).zipped.foreach(_.io.din := _)
  uramPP(1).foreach(_.io.waddr := waddr.value)
  uramPP(1).foreach(_.io.we := false.B)
  (uramPP(1), qs.map(_.bits)).zipped.foreach(_.io.din := _)

  when(qs.map(_.valid).reduce(_ & _)) {
    qs.foreach(_.ready := true.B)
  }.otherwise {
    qs.foreach(_.ready := false.B)
  }
  when(qs.head.fire()) {
    when(state === ping) {
      when(waddr.value === io.length - 1.U) {
        waddr.value := 0.U
      }.otherwise {
        waddr.inc()
      }
      uramPP(0).foreach(_.io.we := true.B)
    }.elsewhen(state === pong) {
      when(waddr.value === io.length - 1.U) {
        waddr.value := 0.U
      }.otherwise {
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

  when(canOut) {
    when(qs.head.fire()) {
      when(waddr.value === io.length - 1.U) {
        outAddr.value := 0.U
      }.otherwise {
        outAddr.inc()
        valid := true.B
      }
      io.outValid := true.B

      val temp0 = Wire(UInt((p(AccW) * p(MaxChannel)).W))
      val temp1 = Wire(UInt((p(AccW) * p(MaxChannel)).W))
      when(io.forceOut){
        when(lastRowValid === 0.U){
          temp0 := qs(0).bits
          temp1 := qs(1).bits
        }.elsewhen(lastRowValid === 1.U){
          temp0 := qs(0).bits
          temp1 := 0.U
        }.elsewhen(lastRowValid === 2.U){
          temp0 := qs(0).bits
          temp1 := qs(1).bits
        }.otherwise{
          temp0 := 0xF.U
          temp1 := 0xF.U
        }
      }.otherwise{
        temp0 := qs(0).bits
        temp1 := qs(1).bits
      }
      when(state === pong){
        (io.inputRowDataOut, uramPP(0).map(_.io.dout) :+ temp0 :+ temp1).zipped.foreach(
          _ := _
        )
      }.elsewhen(state === ping){
        (io.inputRowDataOut, uramPP(1).map(_.io.dout) :+ temp0 :+ temp1).zipped.foreach(
          _ := _
        )
      }
    }
  }

  when((state === last) & (lastRowValid === 0.U) ) {
    when(outAddr.value === io.length) {
      outAddr.value := 0.U
    }.otherwise {
      outAddr.inc()
    }
    when(remainder === 0.U) {
      io.outValid := 0.U
    }.otherwise {
      io.outValid := outAddr.value =/= 0.U
    }
    io.outAddr := outAddr.value - 1.U
    when(lastState === ping) {
      (io.inputRowDataOut, uramPP(0).map(_.io.dout) :+ 0.U :+ 0.U).zipped.foreach(_ := _)
    }.elsewhen(lastState === pong) {
      (io.inputRowDataOut, uramPP(1).map(_.io.dout) :+ 0.U :+ 0.U).zipped.foreach(_ := _)
    }
//    for (i <- 0 until io.inputRowDataOut.length){
//      when(remainder >= (io.inputRowDataOut.length.U - i.U)){
//        io.inputRowDataOut(i) := 0.U
//      }
//    }
  }
}

class NewWB(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    // left
    val dins = Vec(p(Shape)._2, Flipped(Decoupled(SInt(p(OSumW).W))))
    val rowLength = Input(UInt(p(URAMKey).addrW.W))
    val forceOut = Input(Bool())
    // right
    val inputRowDataOut = Output(Vec(p(Shape)._2 + p(FilterSize) - 1, UInt((p(AccW) * p(MaxChannel)).W)))
    val outValid = Output(Bool())
    val outAddr = Output(UInt(p(URAMKey).addrW.W))
    val last = Output(Bool())

    val done = Output(Bool())
  })
  val one2Maxcs = Seq.fill(p(Shape)._2)(Module(new One2MaxChannel))
  (one2Maxcs, io.dins).zipped.foreach(_.io.din <> _)
  one2Maxcs.foreach(_.io.rowLength := io.rowLength)
  val reorder = Module(new MaxChannelReorder)
  io.last := reorder.io.last
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

object GetVeilogMaxPoolingOut extends App {
  implicit val p = new SmallWidthConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_maxpooling_verilog"), () => new MaxPoolingOut())
}
