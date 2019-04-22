package myutil

import java.io.File
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class Test extends Module{
  val io = IO(new Bundle{
    val we = Input(UInt(1.W))
    val addr = Input(UInt(8.W))
    val din = Input(SInt(16.W))
    val dout = Output(SInt(16.W))
  })
  val ram = Module(new DW_ram_r_w_s_dff())
  ram.io.clk := clock
  ram.io.rst_n := reset.asUInt() + 1.U
  ram.io.cs_n := 0.U
  ram.io.wr_n := io.we
  ram.io.rd_addr := io.addr
  ram.io.wr_addr := io.addr
  ram.io.data_in := io.din
  io.dout := ram.io.data_out
}

object getVerilog extends App {
  println("generate pe.PE verilog")
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new Test)
}

class Tester(c: Test) extends PeekPokeTester(c) {
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
}

class TesterTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_ram_vcd", "--top-name", "make_Test_vcd",
        "--backend-name", "verilator"),
      () => new Test
    ) {
      c => new Tester(c)
    } should be(true)
    new File("test_run_dir/make_ram_vcd/Test.vcd").exists should be(true)
  }
}
