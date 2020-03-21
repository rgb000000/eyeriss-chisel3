package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import config.DefaultConfig


class BRAMTests(c:BRAMTestTop) extends PeekPokeTester(c){
  poke(c.io.len, 32)
  poke(c.io.addr, -1)
  poke(c.io.go, 0)
  step(1)
  poke(c.io.go, 1)
  step(1)
  step(10)
  poke(c.io.dout.ready, 1)
  step(10)
  poke(c.io.dout.ready, 0)
  step(10)
  poke(c.io.dout.ready, 1)
  step(100)
}

class BRAMTester extends ChiselFlatSpec{
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_BRAMTest_vcd",
        "--top-name", "make_BRAMTest_vcd",
        "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new BRAMTestTop
    ) {
      c => new BRAMTests(c)
    } should be(true)
  }
}
