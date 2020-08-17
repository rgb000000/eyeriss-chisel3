package myutil

import chisel3._
import chisel3.util._

class Ten2one extends Module{
  val io = IO(new Bundle{
    val in = Input(Vec(10, SInt(16.W)))
    val out = Output(UInt(8.W))
  })

  def two2one(a: Seq[Vec[SInt]]): Vec[SInt] = {
    if(a.length == 2){
      Mux(a(0)(0) > a(1)(0), a(0), a(1))
    }else if(a.length == 1) {
      a(0)
    }else{
      val left = two2one(a.slice(0, (a.length/2).toInt))
      val right = two2one(a.slice((a.length/2).toInt, a.length))
      Mux(left(0) > right(0), left, right)
    }
  }

  val index = VecInit(
    0.S(8.W), 1.S(8.W), 2.S(8.W), 3.S(8.W), 4.S(8.W), 5.S(8.W), 6.S(8.W), 7.S(8.W), 8.S(8.W), 9.S(8.W))
  val inid = (io.in, index).zipped.map(VecInit(_, _))

  io.out := two2one(inid)(1).asUInt()
}

object GetVerilogTen2One extends App {
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_Ten2One_test"), () => new Ten2one())
}

object Ziptest extends App{
  val a = List(1,2,3,4,5)
  println(a.zipWithIndex)
}