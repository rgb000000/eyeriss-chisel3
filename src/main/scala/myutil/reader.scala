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

object GetVerilogReader extends App{
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Reader_test"), () => new Reader)
}
