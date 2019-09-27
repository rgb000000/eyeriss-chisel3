package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._

@chiselName
class maxPooling(val cols: Int = 32, val stride: Int = 2, channelOut:Int = 16) extends Module {
  val io = IO(new Bundle {
    val din = Vec(cols, Flipped(DecoupledIO(SInt(8.W))))
    val dout = Vec(cols/2, DecoupledIO(SInt(8.W)))
    val channelOutNum = Input(UInt(8.W))
    val peaDone = Input(UInt(1.W))
    val allDone = Output(UInt(1.W))
  })

  val ud :: lr :: udandout :: last :: end :: Nil = Enum(5)

  val channelOut_1 = WireInit(io.channelOutNum - 1.U)

  val allDone_reg = RegInit(0.asUInt(1.W))
  io.allDone := allDone_reg

  val din = io.din.grouped(2).toList
  for (i <- din.indices) {
    val qIn = Wire(DecoupledIO(SInt(8.W)))
    val q = Queue(qIn, 2)
    val state = RegInit(ud)
    dontTouch(state)
    val out = RegInit(VecInit(Seq.fill(channelOut)(0.asSInt(8.W))))
    val out2 = RegInit(VecInit(Seq.fill(channelOut)(0.asSInt(8.W))))
    val channelCnt = Counter(channelOut)
    din(i)(0).ready := qIn.ready
    din(i)(1).ready := qIn.ready
    io.dout(i) <> q
    qIn.bits := 0.S
    qIn.valid := 0.U
    val cnt = Counter(256)
    val zcnt = WireInit(cnt.value)
    dontTouch(zcnt)
    switch(state) {
      is(ud) {
        when(din(i)(0).fire() & din(i)(1).fire()) {
          out(channelCnt.value) := Mux(din(i)(0).bits > din(i)(1).bits, din(i)(0).bits, din(i)(1).bits)
          when(channelCnt.value === channelOut_1){
            state := lr
            channelCnt.value := 0.U
          }.otherwise{
            channelCnt.inc()
          }
        }
      }
      is(lr) {
        when(din(i)(0).fire() & din(i)(1).fire()) {
          out2(channelCnt.value) := Mux(din(i)(0).bits > din(i)(1).bits, din(i)(0).bits, din(i)(1).bits)
          when(channelCnt.value === channelOut_1){
            channelCnt.value := 0.U
            when(cnt.value === ((cols / 2 - 1).asUInt() * io.channelOutNum).asUInt()) {
              state := last
            }.otherwise{
              state := udandout
            }
          }.otherwise{
            channelCnt.inc()
          }
        }
      }
      is(udandout) {
        when(din(i)(0).fire() & din(i)(1).fire()) {
          when(channelCnt.value === channelOut_1){
            state := lr
            channelCnt.value := 0.U
          }.otherwise{
            channelCnt.inc()
          }
          qIn.bits := Mux(out(channelCnt.value) > out2(channelCnt.value), out(channelCnt.value), out2(channelCnt.value))
          qIn.valid := 1.U
          cnt.inc()
          out(channelCnt.value) := Mux(din(i)(0).bits > din(i)(1).bits, din(i)(0).bits, din(i)(1).bits)
        }
      }
      is(last) {
        when(channelCnt.value === channelOut_1){
          state := end
          channelCnt.value := 0.U
        }.otherwise{
          channelCnt.inc()
        }
        qIn.bits := Mux(out(channelCnt.value) > out2(channelCnt.value), out(channelCnt.value), out2(channelCnt.value))
        qIn.valid := 1.U
      }
      is(end) {
        when(io.peaDone === 1.U){
          allDone_reg := 1.U
        }.otherwise{
          allDone_reg := 0.U
        }

      }

    }
  }


}
