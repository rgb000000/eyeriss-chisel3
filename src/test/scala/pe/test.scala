package pe

import java.io.File
import breeze.linalg._
import breeze.linalg.{DenseMatrix, DenseVector}
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import simulator._

class Tester(c: Test) extends PeekPokeTester(c) {
  poke(c.io.in, -1)
  step(1)
  poke(c.io.in, -2)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
}

class TesterTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_Test_vcd", "--top-name", "make_Test_vcd",
        "--backend-name", "verilator"),
      () => new Test
    ) {
      c => new Tester(c)
    } should be(true)
    new File("test_run_dir/make_Test_vcd/Test.vcd").exists should be(true)
  }
}

