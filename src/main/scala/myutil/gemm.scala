// Gemm PE
package myutil

import chisel3._
import chisel3.util._

import scala.math.Numeric.FloatAsIfIntegral

class MAC(val w: Int = 8) extends Module{
  val io = IO(new Bundle{
    val a = Flipped(Decoupled(SInt(w.W)))
    val b = Flipped(Decoupled(SInt(w.W)))
    val out = Output(SInt((w*2).W))

    val nextA = Decoupled(SInt(w.W))
  })

  io.a.ready := 1.U
  io.b.ready := io.a.fire()

  val result = RegInit(0.S((2*w).W))
  dontTouch(result)
  io.out := result

  when(io.a.fire() & io.b.fire()){
    result := result + io.a.bits * io.b.bits
  }

  io.nextA.valid := RegNext(io.a.valid, 0.U)
  io.nextA.bits := RegNext(io.a.bits, 0.S)
}

class MacN(val n:Int = 12, val w:Int = 8) extends Module{
  val io = IO(new Bundle {
    val an = Flipped(Decoupled(Vec(n, SInt(w.W))))
    val bn = Flipped(Decoupled(Vec(n, SInt(w.W))))
    val outn = Output(Vec(n, SInt((w*2).W)))

    val nextAn = Decoupled(Vec(n, SInt(w.W)))
  })

  val macn = List.fill(n)(Module(new MAC(w)))
  // a
  (macn, io.an.bits).zipped.foreach(_.io.a.bits := _)
  macn.foreach(_.io.a.valid := io.an.valid)
  io.an.ready := macn.head.io.a.ready
  // b
  (macn, io.bn.bits).zipped.foreach(_.io.b.bits := _)
  macn.foreach(_.io.b.valid := io.bn.valid)
  io.bn.ready := macn.head.io.b.ready
  // nextA
  (io.nextAn.bits, macn).zipped.foreach(_ := _.io.nextA.bits)
  io.nextAn.valid := macn.head.io.nextA.valid
  macn.foreach(_.io.nextA.ready := io.nextAn.ready)
  // output
  (io.outn, macn).zipped.foreach(_ := _.io.out)
}

class GEMMn(val n:Int = 12, val row:Int = 1, val col:Int = 10, val w:Int = 8) extends Module{
  val io = IO(new Bundle{
    val inA = Vec(row, Flipped(Decoupled(Vec(n, SInt(w.W)))))
    val inB = Vec(col, Flipped(Decoupled(Vec(n, SInt(w.W)))))

    val out = Output(Vec(col, Vec(n, SInt((w*2).W))))
  })

  val gemm = Seq.tabulate(row, col)((x, y) => {
    Module(new MacN(n, w))
  })

  (gemm.map(_.head), io.inA).zipped.foreach(_.io.an <> _)
  (gemm.last, io.inB).zipped.foreach(_.io.bn <> _)
  (io.out, gemm.head).zipped.foreach(_ := _.io.outn)
  gemm.map(_.last).foreach(_.io.nextAn.ready := 1.U)

  for (i <- 1 until col){
    (gemm.map(_ (i)), gemm.map(_ (i-1))).zipped.foreach(_.io.an <> _.io.nextAn)
  }
}

object GetVerilogGEMM extends App {
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_GEMM_test"), () => new GEMMn())
}
