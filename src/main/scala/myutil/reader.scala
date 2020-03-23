// reader
package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._
import pe.{PEConfigReg}

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

  when(state === idle & io.go){
    len := io.length
  }

  io.read.cmd.valid := (state === work)
  io.read.cmd.bits.addr := addr
  // brust_len = len - 1
  when(isNotlastRead){
    io.read.cmd.bits.len := ((1 << p(ShellKey).memParams.lenBits) - 1).asUInt()
  }.otherwise{
    io.read.cmd.bits.len := len - 1.U
  }
  when(io.read.cmd.fire()){
    when(isNotlastRead){
      len := len - (1 << p(ShellKey).memParams.lenBits).asUInt()
      addr := addr + ((1 << p(ShellKey).memParams.lenBits) << log2Ceil(p(ShellKey).memParams.dataBits)).asUInt()
    }.otherwise{
      len := len
      addr := addr + (len << log2Ceil(p(ShellKey).memParams.dataBits)).asUInt()
    }
  }

  // translate data to PEArray
  io.dout <> io.read.data
}

class BRAMFilterReader(isFilter: Boolean = true)(implicit p: Parameters) extends Module{
  require(isFilter == true)
  val io = IO(new Bundle{
    val r = Flipped(new BRAMInterface(p(BRAMKey).dataW))
    val dout = DecoupledIO(UInt(p(BRAMKey).dataW.W))
    val go = Input(Bool())

    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))

    val fid = if(isFilter) Some(Output(UInt(p(Shape)._1.W))) else None
  })

  val idle :: read :: done :: Nil = Enum(3)
  val state = RegInit(idle)
  val len = RegInit(0.U(16.W))

  val addr = RegInit(0.U(p(BRAMKey).addrW.W))
  val we = RegInit(false.B)
  val valid = RegInit(false.B)
  val validDelay = RegNext(valid)

  val qIn = Wire(DecoupledIO(UInt(p(BRAMKey).dataW.W)))
  val q = Queue(qIn)
  io.dout <> q

  val pIn = Wire(DecoupledIO(UInt(p(BRAMKey).dataW.W)))
  pIn.bits := io.r.dout
  pIn.valid := validDelay & (!qIn.ready)
  val dataPool = Queue(pIn)

  when(dataPool.valid){
    qIn <> dataPool
  }.otherwise{
    qIn.valid := validDelay
    qIn.bits := io.r.dout
    dataPool.ready := 0.U
  }

  switch(state){
    is(idle){
      when(io.go){
        state := read
      }
    }
    is(read){
      when(len === 1.U){
        state := done
      }
    }
    is(done){

    }
  }

  when(state === idle){
    len := io.len
    addr := io.addr - 1.U
  }

  when(state === read){
    when(qIn.ready & !dataPool.valid){
      addr := addr + 1.U
      valid := 1.U
      len := len - 1.U
    }.otherwise{
      addr := addr
      valid := 0.U
      len := len
    }
  }

  when(state === done){
    valid := 0.U
  }

  io.r.addr := addr
  io.r.din := 0.U
  io.r.we := we

  if(isFilter){
    val fcnt = Counter(3)
    val id = RegInit(1.asUInt(p(Shape)._1.W))
    io.fid.get := id
    when(io.dout.fire()){
      when(fcnt.value === 2.U){
        id := id << 1.U
      }
      fcnt.inc()
    }
  }
}

class BRAMImgReader(implicit p: Parameters) extends Module{
  val n = p(Shape)._1 + p(Shape)._2 - 1
  val io = IO(new Bundle{
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

  val qIn = Wire(DecoupledIO(UInt((n*p(BRAMKey).dataW).W)))
  val q = Queue(qIn)
  val widthcvt = Module(new widthCvtWire(n*p(BRAMKey).dataW, p(BRAMKey).dataW))
  widthcvt.io.in := q.bits
  io.doutSplit.valid := q.valid
  (io.doutSplit.bits, widthcvt.io.out).zipped.foreach(_ := _)
  q.ready := io.doutSplit.ready


  val pIn = Wire(DecoupledIO(UInt(p(BRAMKey).dataW.W)))
  pIn.bits := io.r.dout
  pIn.valid := validDelay & (!qIn.ready)
  val dataPool = Queue(pIn)

  when(dataPool.valid){
    qIn <> dataPool
  }.otherwise{
    qIn.valid := validDelay
    qIn.bits := io.r.dout
    dataPool.ready := 0.U
  }

  switch(state){
    is(idle){
      when(io.go){
        state := read
      }
    }
    is(read){
      when(len === 1.U){
        state := done
      }
    }
    is(done){

    }
  }

  when(state === idle){
    len := io.len
    addr := io.addr - 1.U
  }

  when(state === read){
    when(qIn.ready & !dataPool.valid){
      addr := addr + 1.U
      valid := 1.U
      len := len - 1.U
    }.otherwise{
      addr := addr
      valid := 0.U
      len := len
    }
  }

  when(state === done){
    valid := 0.U
  }

  io.r.addr := addr
  io.r.din := 0.U
  io.r.we := we
}


class BRAMFilterReaderTestTop(isFilter: Boolean = true)(implicit p: Parameters) extends Module{
  require(isFilter == true)
  val io = IO(new Bundle{
    val dout = DecoupledIO(UInt(p(BRAMKey).dataW.W))
    val go = Input(Bool())
    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))
    val fid = if(isFilter) Some(Output(UInt(p(Shape)._1.W))) else None
  })
  val bram = Module(new BRAM(p(BRAMKey).dataW))
  val reader = Module(new BRAMFilterReader(isFilter))
  bram.io <> reader.io.r
  reader.io.go := io.go
  reader.io.len := io.len
  reader.io.addr := io.addr
  io.dout <> reader.io.dout
  if(isFilter){
    io.fid.get := reader.io.fid.get
  }
}

class BRAMImgReaderTestTop(implicit p: Parameters) extends Module{
  val n = p(Shape)._1 + p(Shape)._2 -1
  val io = IO(new Bundle{
    val doutSplit = DecoupledIO(Vec(n, UInt(p(BRAMKey).dataW.W)))
    val go = Input(Bool())

    val len = Input(UInt(16.W))
    val addr = Input(UInt(p(BRAMKey).addrW.W))
  })
  val bram = Module(new BRAM(p(BRAMKey).dataW * n))
  val reader = Module(new BRAMImgReader)
  bram.io <> reader.io.r
  reader.io.go := io.go
  reader.io.len := io.len
  reader.io.addr := io.addr
  io.doutSplit <> reader.io.doutSplit
}

object GetVerilogReader extends App{
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Reader_test"), () => new BRAMImgReader)
}
