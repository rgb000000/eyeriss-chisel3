// reader
package myutil

import chisel3._
import chisel3.util._
import config._
import ram._

class Reader(val row: Int = 0, val addrInit: Int = 0)(implicit val p: Parameters) extends Module {
  val io = IO(new Bundle {
    val read = new VMEReadMaster
    val dout = DecoupledIO(UInt(p(ShellKey).memParams.dataBits.W))
    val length = Input(UInt(16.W))
    val go = Input(Bool())
  })

  val cmd = Reg(new VMECmd)
  val len = Reg(UInt(16.W))
  val addr = RegInit(addrInit.asUInt(p(ShellKey).memParams.addrBits.W))
  val isNotlastRead = Wire(Bool())
  when(len > (1 << p(ShellKey).memParams.lenBits).asUInt()) {
    isNotlastRead := true.B
  }.otherwise {
    isNotlastRead := false.B
  }

  val idle :: work :: Nil = Enum(2)
  val state = RegInit(idle)

  // request generator
  switch(state) {
    is(idle) {
      when(io.go) {
        state := work
      }
    }
    is(work) {
      when(!isNotlastRead) {
        state := idle
      }
    }
  }

  when(state === idle & io.go) {
    len := io.length
  }

  io.read.cmd.valid := (state === work)
  io.read.cmd.bits.addr := addr
  // brust_len = len - 1
  when(isNotlastRead) {
    io.read.cmd.bits.len := ((1 << p(ShellKey).memParams.lenBits) - 1).asUInt()
  }.otherwise {
    io.read.cmd.bits.len := len - 1.U
  }
  when(io.read.cmd.fire()) {
    when(isNotlastRead) {
      len := len - (1 << p(ShellKey).memParams.lenBits).asUInt()
      addr := addr + ((1 << p(ShellKey).memParams.lenBits) << log2Ceil(p(ShellKey).memParams.dataBits)).asUInt()
    }.otherwise {
      len := len
      addr := addr + (len << log2Ceil(p(ShellKey).memParams.dataBits)).asUInt()
    }
  }

  // translate data to PEArray
  io.dout <> io.read.data
}

class BRAMFilterReader(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new BRAMInterface(p(BRAMKey).dataW))
    val dout = DecoupledIO(UInt(p(BRAMKey).dataW.W))

    val addr = Input(UInt(p(BRAMKey).addrW.W))

    val fid = Output(UInt(p(Shape)._1.W))
    val done = Output(Bool())

    val go = Input(Bool())
    val len = Input(UInt(16.W))
    val inChannelGroup = Input(UInt(8.W))
  })

  val idle :: read :: done :: Nil = Enum(3)
  val state = RegInit(idle)
  val len = RegInit(0.U(16.W))

  val baseAddr = RegInit(0.U(p(BRAMKey).addrW.W))
  val addr = RegInit(0.U(p(BRAMKey).addrW.W))
  val inChannelGroup = Reg(UInt(8.W))
  val we = RegInit(false.B)
  val valid = RegInit(false.B)
  val validDelay = RegNext(valid)

  val qIn = Wire(DecoupledIO(UInt(p(BRAMKey).dataW.W)))
  val q = Queue(qIn)
  io.dout <> q

  // poolIn
  val pIn = Wire(DecoupledIO(UInt(p(BRAMKey).dataW.W)))
  pIn.bits := io.r.dout
  val dataPool = Queue(pIn)
  pIn.valid := (validDelay & (!qIn.ready)) | (validDelay & qIn.ready & dataPool.valid)
  val test = WireInit(validDelay & qIn.ready & dataPool.valid)
  dontTouch(test)

  // when dataPool have data, get data from it instead of from ram
  when(dataPool.valid) {
    qIn <> dataPool
  }.otherwise {
    qIn.valid := validDelay
    qIn.bits := io.r.dout
    dataPool.ready := 0.U
  }

  switch(state) {
    is(idle) {
      when(io.go) {
        state := read
      }
    }
    is(read) {
      when(len === 1.U) {
        state := done
      }
    }
    is(done) {

    }
  }

  when(state === idle) {
    len := io.len
    addr := io.addr - 1.U
    baseAddr := io.addr
    inChannelGroup := io.inChannelGroup
  }

  // addr logic
  when(state === read) {
    when(qIn.ready & !dataPool.valid) {
      addr := addr + 1.U
      valid := 1.U
      len := len - 1.U
    }.otherwise {
      addr := addr
      valid := 0.U
      len := len
    }
  }

  when(state === done) {
    valid := 0.U
  }

  // fid logic
  val idCnt = Counter(48)       // 3 * 16, outChannel first
  val id = RegInit(1.asUInt(p(Shape)._1.W))
  io.fid := id
  when(io.dout.fire()) {
    when(idCnt.value === (inChannelGroup * 3.U - 1.U)) {
      id := id << 1.U
      idCnt.value := 0.U
    }.otherwise{
      idCnt.inc()
    }
  }

  io.r.addr := addr
  io.r.din := 0.U
  io.r.we := we
  io.done := state === done
}

class BRAMImgReaderWithPadding (implicit p: Parameters) extends Module {
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val io = IO(new Bundle {
    val r = Flipped(new BRAMInterface(n * p(BRAMKey).dataW))
    val doutSplit = DecoupledIO(Vec(n, UInt(p(BRAMKey).dataW.W)))
    val go = Input(Bool())

    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))

    // 0 1 2 -> top middle bottom
    val topOrBottom = Input(UInt(2.W))
    val inChannel = Input(UInt(8.W))
    val done = Output(Bool())
  })

  // uram control signal
  val addr = RegInit(0.U(p(BRAMKey).addrW.W))
  val we = RegInit(false.B)
  val valid = RegInit(false.B)
  val validDelay = RegInit(false.B)
  validDelay := valid
  val padding = WireInit(false.B)

  // temp uram
  val tempRow = Module(new URAM(p(BRAMKey).addrW, p(BRAMKey).dataW))
  val tempRowWE = RegInit(false.B)
  tempRowWE := validDelay
  // counter backward 3rd
  val tempRowWdata = RegNext(io.r.dout((n-2)*p(BRAMKey).dataW - 1, (n-3)*p(BRAMKey).dataW))
  val tempRowAddr = RegNext(RegNext(addr))
  tempRow.io.we := tempRowWE
  tempRow.io.waddr := tempRowAddr
  tempRow.io.din := tempRowWdata

  val outData = Wire(UInt((n * p(BRAMKey).dataW).W))
  when(io.topOrBottom === 0.U){
    // top
    outData := (io.r.dout << p(BRAMKey).dataW.U).asUInt() + 0.asUInt(p(BRAMKey).dataW.W)
  }.elsewhen(io.topOrBottom === 1.U){
    // middle
    outData := (io.r.dout << p(BRAMKey).dataW.U).asUInt() + tempRow.io.dout
  }.elsewhen(io.topOrBottom === 2.U){
    // bottom
    outData := (io.r.dout << p(BRAMKey).dataW.U).asUInt() + 0.asUInt(p(BRAMKey).dataW.W)
  }.otherwise{
    // error
    outData := 0.U
  }

  // output queue
  val qIn = Wire(DecoupledIO(UInt((n * p(BRAMKey).dataW).W)))
  val q = Queue(qIn, 16)
  val widthcvt = Module(new widthCvtWire(n * p(BRAMKey).dataW, p(BRAMKey).dataW))
  widthcvt.io.in := q.bits
  io.doutSplit.valid := q.valid
  (io.doutSplit.bits, widthcvt.io.out).zipped.foreach(_ := _)
  q.ready := io.doutSplit.ready
  // output dataPool
  val pIn = Wire(DecoupledIO(UInt((n * p(BRAMKey).dataW).W)))
  pIn.bits := outData
  val dataPool = Queue(pIn)
  pIn.valid := ((validDelay | padding) & (!qIn.ready)) | ((validDelay | padding) & qIn.ready & dataPool.valid)

  when(dataPool.valid) {
    qIn <> dataPool
  }.otherwise {
    qIn.valid := validDelay | padding
    qIn.bits := outData
    dataPool.ready := 0.U
  }

  val outLen = Counter(512)


  io.done := 0.U
  val channleCnt = Counter(256)
  val idle :: left :: middle :: right :: Nil = Enum(4)
  val state = RegInit(idle)
  switch(state){
    is(idle){
      when(io.go){
        state := left
      }
    }
    is(left){
      when(qIn.fire() | pIn.fire()){
        when(channleCnt.value === io.inChannel - 1.U){
          channleCnt.value := 0.U
          state := middle
        }.otherwise{
          channleCnt.inc()
        }
      }
    }
    is(middle){
      when(outLen.value === io.len){
        state := right
      }
    }
    is(right) {
      when(qIn.fire() | pIn.fire()) {
        when(channleCnt.value === io.inChannel - 1.U) {
          channleCnt.value := 0.U
          state := middle
          state := idle
          io.done := 1.U
        }.otherwise {
          channleCnt.inc()
        }
      }
    }
  }
  // state === idle logic
  when(state === idle){
    addr := io.addr - 1.U
  }
  // state === top logic
  when(state === left){
    outData := 0.U
    padding := 1.U
    valid := 0.U
  }.elsewhen(state === middle){
    when((qIn.ready & !dataPool.valid) & (outLen.value =/= io.len)) {
      addr := addr + 1.U
      valid := 1.U
      outLen.inc()
    }.otherwise {
      addr := addr
      valid := 0.U
    }
  }.elsewhen(state === right){
    when(qIn.ready & !dataPool.valid){
      outData := 0.U
      padding := 1.U
      valid := 0.U
    }.otherwise{
      padding := 0.U
      valid := 0.U
    }
  }

  io.r.addr := addr
  io.r.din := 0.U
  io.r.we := we
  tempRow.io.raddr := addr
}

class BRAMImgReader(implicit p: Parameters) extends Module {
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val io = IO(new Bundle {
    val r = Flipped(new BRAMInterface(n * p(BRAMKey).dataW))
    val doutSplit = DecoupledIO(Vec(n, UInt(p(BRAMKey).dataW.W)))
    val go = Input(Bool())

    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))
  })

  val idle :: read :: done :: Nil = Enum(3)
  val state = RegInit(idle)
  val len = RegInit(0.U(16.W))

  val addr = RegInit(0.U(p(BRAMKey).addrW.W))
  val we = RegInit(false.B)
  val valid = RegInit(false.B)
  val validDelay = RegNext(valid)

  val qIn = Wire(DecoupledIO(UInt((n * p(BRAMKey).dataW).W)))
  val q = Queue(qIn, 16)
  val widthcvt = Module(new widthCvtWire(n * p(BRAMKey).dataW, p(BRAMKey).dataW))
  widthcvt.io.in := q.bits
  io.doutSplit.valid := q.valid
  (io.doutSplit.bits, widthcvt.io.out).zipped.foreach(_ := _)
  q.ready := io.doutSplit.ready


  val pIn = Wire(DecoupledIO(UInt((n * p(BRAMKey).dataW).W)))
  pIn.bits := io.r.dout
  val dataPool = Queue(pIn)
  pIn.valid := (validDelay & (!qIn.ready)) | (validDelay & qIn.ready & dataPool.valid)
  val test = WireInit(validDelay & qIn.ready & dataPool.valid)
  dontTouch(test)

  when(dataPool.valid) {
    qIn <> dataPool
  }.otherwise {
    qIn.valid := validDelay
    qIn.bits := io.r.dout
    dataPool.ready := 0.U
  }

  switch(state) {
    is(idle) {
      when(io.go) {
        state := read
      }
    }
    is(read) {
      when(len === 1.U) {
        state := done
      }
    }
    is(done) {

    }
  }

  when(state === idle) {
    len := io.len
    addr := io.addr - 1.U
  }

  when(state === read) {
    when(qIn.ready & !dataPool.valid) {
      addr := addr + 1.U
      valid := 1.U
      len := len - 1.U
    }.otherwise {
      addr := addr
      valid := 0.U
      len := len
    }
  }

  when(state === done) {
    valid := 0.U
  }

  io.r.addr := addr
  io.r.din := 0.U
  io.r.we := we
}


class BRAMFilterReaderTestTop(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val dout = DecoupledIO(UInt(p(BRAMKey).dataW.W))
    val go = Input(Bool())
    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))
    val inChannelGroup = Input(UInt(8.W))
    val fid = Output(UInt(p(Shape)._1.W))
  })
  val bram = Module(new BRAM(p(BRAMKey).dataW))
  val reader = Module(new BRAMFilterReader)
  reader.io.inChannelGroup := io.inChannelGroup
  bram.io <> reader.io.r
  reader.io.go := io.go
  reader.io.len := io.len
  reader.io.addr := io.addr
  io.dout <> reader.io.dout
  io.fid := reader.io.fid
}

class BRAMImgReaderTestTop(implicit p: Parameters) extends Module {
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val io = IO(new Bundle {
    val doutSplit = DecoupledIO(Vec(n, UInt(p(BRAMKey).dataW.W)))
    val go = Input(Bool())

    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))

    // 0 1 2 -> top middle bottom
    val topOrBottom = Input(UInt(2.W))
    val inChannel = Input(UInt(8.W))
  })
  val bram = Module(new BRAM(p(BRAMKey).dataW * n, p(FeatureMEMPath)))
  val reader = Module(new BRAMImgReaderWithPadding)
  bram.io <> reader.io.r
  reader.io.go := io.go
  reader.io.len := io.len
  reader.io.addr := io.addr
  reader.io.inChannel := io.inChannel
  reader.io.topOrBottom := io.topOrBottom
  io.doutSplit <> reader.io.doutSplit
}

object GetVerilogReader extends App {
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Reader_test"), () => new BRAMImgReader)
}
