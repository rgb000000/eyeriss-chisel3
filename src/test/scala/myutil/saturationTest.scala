package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class SaturationTest(c: Saturation) extends PeekPokeTester(c) {
  def sw(x: Int): Int = {
    val tmp = if (x >= 0) {
      x / 4.0 + 0.5
    } else {
      x / 4.0 - 0.5
    }
    if (tmp >= 127) {
      127
    } else if (tmp <= -128) {
      -128
    } else {
      tmp.toInt
    }
  }
  var in = 11604
  poke(c.io.dataIn, in)
  expect(c.io.dataOut, sw(in))
  println(in.toString + ":" + peek(c.io.dataOut).toString() + " <---> " + sw(in))
  step(1)
  val R = scala.util.Random
  for(i <- Range(0, 100)){
//    val in = R.nextInt(32767) - 16384
    val in = R.nextInt(1024) - 512
    poke(c.io.dataIn, in)
    expect(c.io.dataOut, sw(in))
    println(in.toString + ":" + peek(c.io.dataOut).toString() + " <---> " + sw(in))
    step(1)
  }
}

class SaturationTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_saturation_vcd",
        "--top-name", "make_saturation_vcd", "--backend-name", "verilator"),
      () => new Saturation()
    ) {
      c => new SaturationTest(c)
    } should be(true)
  }
}
