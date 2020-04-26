// WB Test
package ram

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import config._

class WBTests(c: NewWB)(implicit p: Parameters) extends PeekPokeTester(c){
  val len = 16
  val in = List.tabulate(p(Shape)._2)(i => i)
  poke(c.io.rowLength, len)
  (c.io.dins, in).zipped.foreach((dutio, input) => {
    poke(dutio.bits, input)
    poke(dutio.valid, 1)
  })
  step(500)
}

class WBTester extends ChiselFlatSpec {
  implicit val p = new SmallWidthConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_wb_vcd",
        "--top-name", "make_wb_vcd",
        "--backend-name", "verilator"
      ),
      () => new NewWB
    ) {
      c => new WBTests(c)
    } should be(true)
  }
}
