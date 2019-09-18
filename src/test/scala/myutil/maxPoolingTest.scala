package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class maxPoolingTest(c:maxPooling) extends PeekPokeTester (c) {
  def set(data:Int = 1) = {
    for(i <- c.io.din){
      poke(i.valid, 1)
      poke(i.bits, data)
    }
  }
  def unset() = {
    for(i <- c.io.din){
      poke(i.valid, 0)
      poke(i.bits, 0)
    }
  }
  c.io.dout.map(_.ready).foreach(poke(_, 1))
  unset()
  step(10)
  set(1)
  step(1)
  unset()
  step(10)
  set(2)
  step(1)
  unset()
  step(10)
  set(3)
  step(1)
  unset()
  step(10)
  set(4)
  step(1)
  unset()
  step(100)
}

class maxPoolingTester extends ChiselFlatSpec{
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_maxPooling_vcd",
        "--top-name", "make_maxPooling_vcd", "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new maxPooling()
    ) {
      c => new maxPoolingTest(c)
    } should be(true)
  }
}