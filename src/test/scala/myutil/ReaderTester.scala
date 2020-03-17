// Reader Tester
package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import config._

class ReaderTests(c: Reader) extends PeekPokeTester(c){
  val length = 555

  poke(c.io.length, length)
  poke(c.io.dout.ready, 1)
  poke(c.io.go, 1)
  poke(c.io.read.cmd.ready, 1)
  poke(c.io.read.data.valid, 1)
  poke(c.io.read.data.bits, 7)
  step(1)
  poke(c.io.go, 0)
  step(100)

}

class ReaderTester extends ChiselFlatSpec {
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_Reader_vcd",
        "--top-name", "make_maxPooling_vcd",
        "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new Reader(0, 0)
    ) {
      c => new ReaderTests(c)
    } should be(true)
  }
}
