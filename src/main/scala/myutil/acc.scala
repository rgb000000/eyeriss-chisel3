package myutil

import chisel3._
import chisel3.internal.naming.chiselName
import chisel3.util._
import chisel3.experimental._
import pe._
import myutil._
import config._

class Acc(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(SInt(p(OSumW).W)))
    val bias = Input(SInt(p(BiasW).W))
    val out = DecoupledIO(SInt(p(AccW).W))
    val accState = Input(UInt(p(RegFileW).W))
    val cntLen = Input(UInt(16.W))
  })

  val mem = Mem(512, SInt(p(OSumW).W))
  val addr = Counter(512)
  val biasValue = Reg(SInt(p(BiasW).W))

  val acc :: bias :: output ::Nil = Enum(3)
  val state = RegInit(acc)

  val sagain :: slast :: spush :: Nil = Enum(3)

  val again = WireInit(io.accState === sagain)
  val last = WireInit(io.accState === slast)
  val push = WireInit(io.accState === spush)

  val qIn = Wire(DecoupledIO(SInt(p(AccW).W)))
  qIn.valid := 0.U
  qIn.bits := 0.S
  val q = Queue(qIn, 16)
  io.out <> q


  switch(state){
    is(acc){
      when(last){
        state := bias
      }
    }
    is(bias){
      when(push){
        state := output
      }
    }
    is(output){

    }
  }

  when(state === acc){
    biasValue := io.bias
  }

  io.in.ready := state === acc
  when(addr.value === io.cntLen - 1.U & (io.in.fire() | qIn.fire())){
    when(state === bias){
      state := output
    }
    addr.value := 0.U
  }.elsewhen(io.in.fire() | qIn.fire()){
    addr.inc()
  }

  when(state === acc & io.in.fire()){
    mem.write(addr.value, mem.read(addr.value) + io.in.bits)
  }.elsewhen(state === bias){
    qIn.bits := mem.read(addr.value) + biasValue
    qIn.valid := 1.U
  }
}
object GetVerilogAcc extends App {
  implicit val p = new DefaultConfig
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Acc_test"), () => new Acc)
}
