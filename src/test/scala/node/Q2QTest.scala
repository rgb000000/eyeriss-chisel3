package node

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class Q2QTest(c: Q2Q) extends PeekPokeTester(c) {

  poke(c.io.bigIn.valid, 1)
  poke(c.io.bigIn.bits.dataType, 1)
  for (i <- Range(0, 8)){
    poke(c.io.bigIn.bits.data(i), 0xF0 + i)
  }
  poke(c.io.bigIn.bits.cnt, 8)
  poke(c.io.bigIn.bits.positon.col, 0)
  poke(c.io.bigIn.bits.positon.row, 0)

  poke(c.io.smallOut.ready, 1)

  step(1)
  poke(c.io.bigIn.valid, 0)

  step(1)
  step(1)
  step(1)
  step(1)

  poke(c.io.smallOut.ready, 0)

  step(1)
  step(1)

  poke(c.io.smallOut.ready, 1)
  step(1)
  step(1)
  step(1)

  poke(c.io.smallOut.ready, 0)
  step(1)
  poke(c.io.smallOut.ready, 1)
  step(1)
  Range(0, 50).foreach((x) => {step(1)})

  poke(c.io.bigIn.valid, 1)
  poke(c.io.bigIn.bits.dataType, 1)
  for (i <- Range(0, 8)){
    poke(c.io.bigIn.bits.data(i), 0xA0 + i)
  }
  poke(c.io.bigIn.bits.cnt, 1)
  poke(c.io.bigIn.bits.positon.col, 0)
  poke(c.io.bigIn.bits.positon.row, 0)

  poke(c.io.smallOut.ready, 1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)


}

class Q2QTester extends ChiselFlatSpec{
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_Q2Q_vcd", "--backend-name", "verilator",
        "--top-name", "make_Q2Q_vcd"),
      () => new Q2Q(280, 8)) {
      c => new Q2QTest(c)
    } should be(true)
    //    new File("test_run_dir/make_PEArray_vcd/PEArray.vcd").exists should be(true)
  }
}
