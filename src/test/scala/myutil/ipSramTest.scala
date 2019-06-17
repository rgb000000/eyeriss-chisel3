package myutil

import java.io.File
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}


object getVerilog extends App {
  println("generate pe.PE verilog")
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new SRAM)
}

class Tester(c: SRAM) extends PeekPokeTester(c) {
  def read(a: SRAM, addr: UInt):SInt = {
    poke(a.io.addr, addr)
    poke(a.io.we, 1.U)
    peek(a.ram.io.data_out).asSInt()
  }
  def write(a: SRAM, addr:UInt, data: SInt): Unit ={
    poke(a.io.addr, addr)
    poke(a.io.we, 0.U)
    poke(a.io.din, data)
  }
  poke(c.io.rstLowas, 0.U)
  step(1)
  poke(c.io.rstLowas, 1.U)
  step(1)
  for(i <- Range(0, 256)){
    write(c, i.asUInt(), (i + 1).asSInt())
    step(1)
  }
  for(i <- Range(0, 256)){
    poke(c.io.addr, i)
    poke(c.io.we, 1.U)
    expect(c.io.dout, i + 1)
    step(1)
  }
}

class TesterTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_ram_vcd", "--top-name", "make_Test_vcd",
        "--backend-name", "verilator"),
      () => new SRAM
    ) {
      c => new Tester(c)
    } should be(true)
    new File("test_run_dir/make_ram_vcd/Test.vcd").exists should be(true)
  }
}
