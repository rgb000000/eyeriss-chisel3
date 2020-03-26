// writer
package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._
import config._
import pe.{PEConfigReg}

class Writer(implicit val p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Vec(p(Shape)._2, Flipped(DecoupledIO(SInt(p(AccW).W))))
    val wr = new VMEWriteMaster
  })
  val wr_arb = Module(new Arbiter(SInt(p(AccW).W), p(Shape)._2))
  (wr_arb.io.in, io.in).zipped.foreach(_ <> _)

  val addr = RegInit(VecInit(Seq.fill(p(Shape)._2)(0.U(p(ShellKey).memParams.dataBits.W))))
  val data = Reg(UInt(p(ShellKey).memParams.dataBits.W))
  val chose = Reg(UInt(log2Ceil(p(ShellKey).memParams.dataBits).W))

  val idle :: wCmd :: wData :: Nil = Enum(3)
  val state = RegInit(idle)

  switch(state){
    is(idle){
      when(wr_arb.io.out.fire()){
        state := wCmd
      }
    }
    is(wCmd){
      when(io.wr.cmd.fire()){
        state := wData
      }
    }
    is(wData){
      when(io.wr.ack){
        state := idle
      }
    }
  }

  wr_arb.io.out.ready := state === idle

  when(wr_arb.io.out.fire()){
    data := wr_arb.io.out.bits.asUInt()
    chose := wr_arb.io.chosen
  }

  io.wr.cmd.valid := state === wCmd
  io.wr.cmd.bits.len := 0.U
  io.wr.cmd.bits.addr := addr(wr_arb.io.chosen)

  when(io.wr.data.fire()){
    addr(chose) := addr(chose) + 1.U
  }
  io.wr.data.valid := state === wData
  io.wr.data.bits := data
}

class BRAMWriter(implicit p: Parameters) extends Module{
  val io = IO(new Bundle{
    val in = Vec(p(Shape)._2, Flipped(DecoupledIO(SInt(p(AccW).W))))
    val w = Flipped(new BRAMInterface(p(WriterBRAMW)))
  })

  val addr = Counter(1024)
  io.in.foreach(_.ready := 1.U)
  val concat = Wire(Vec(p(Shape)._2, SInt(p(AccW).W)))
  (concat, io.in).zipped.foreach(_ := _.bits)

  io.w.addr := addr.value
  io.w.din := concat.asUInt()
  when(io.in.map(_.valid).reduce(_&_)){
    io.w.we := 1.U
    addr.inc()
  }.otherwise{
    io.w.we := 0.U
  }
}

object GetVerilogWriter extends App{
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Writer_test"), () => new BRAMWriter)
}
