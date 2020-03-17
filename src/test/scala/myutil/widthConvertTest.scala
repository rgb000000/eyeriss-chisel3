// widthConvert Test
package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import config.DefaultConfig

class widthConvertTests(c: widthConvert) extends PeekPokeTester(c){
  poke(c.io.in.valid, 1)
  poke(c.io.in.bits, 15)
  poke(c.io.out.ready, 1)
  step(15)
  poke(c.io.out.ready, 0)
  step(15)
}

class widthConvertTester extends ChiselFlatSpec {
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_widthCVT_vcd",
        "--top-name", "make_widthCVT_vcd",
        "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new widthConvert
    ) {
      c => new widthConvertTests(c)
    } should be(true)
  }
}
