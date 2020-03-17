// Writer Tester
package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import config._

class WriterTests(c: Writer) extends PeekPokeTester(c){
  poke(c.io.in(0).valid, 1)
  poke(c.io.in(0).bits, 15)
  step(1)
  poke(c.io.wr.cmd.ready, 1)
  step(1)
  poke(c.io.wr.cmd.ready, 0)
  poke(c.io.wr.data.ready, 1)
  step(1)
  poke(c.io.wr.cmd.ready, 0)
  poke(c.io.wr.data.ready, 0)
  poke(c.io.wr.ack, 1)
  step(100)
}

class WriterTester extends ChiselFlatSpec {
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_Writer_vcd",
        "--top-name", "make_Writer_vcd",
        "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new Writer
    ) {
      c => new WriterTests(c)
    } should be(true)
  }
}
