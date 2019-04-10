package pe

import java.io.File
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

object getVerilog extends App{
  println("generate pe.PE verilog")
//  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PE)
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PETesterTop)
//  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new test)
}


class PETest(c: PE) extends PeekPokeTester(c){

  val a = RegInit(3.U(8.W))


  poke(c.io.stateSW, 0)
  step(1)
  step(1)
  step(1)
  step(1)
  poke(c.io.stateSW, 1)
  step(1)
  poke(c.io.filter.valid, 1)
  poke(c.io.filter.bits, 11)
  poke(c.io.img.valid, 1)
  poke(c.io.img.bits, 2)

  step(1)
  poke(c.io.img.bits, 3)
  poke(c.io.filter.bits, 4)
  step(1)
  poke(c.io.img.bits, 5)
  poke(c.io.filter.bits, 6)

  step(1)
  poke(c.io.img.bits, 7)
  poke(c.io.filter.bits, 8)

  step(1)
  poke(c.io.img.bits, 5)
  poke(c.io.filter.bits, 6)

  step(1)
  poke(c.io.stateSW, 2)
  step(1)
  step(1)
  poke(c.io.filter.valid, 0)
  poke(c.io.oSum.ready, 1)


  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
}

class PETopTester(c: PETesterTop) extends PeekPokeTester(c){
  poke(c.io.stateSW, 0)
  step(1)
  // first: let PE in idle and input data to FIFO
  for(i <- Range(1,6)){
    poke(c.io.fIn.valid, 1)
    poke(c.io.iIn.valid, 1)
    poke(c.io.fIn.bits, i)
    poke(c.io.iIn.bits, i)
    step(1)
  }
  poke(c.io.fIn.valid, 0)
  poke(c.io.iIn.valid, 0)
  step(1)
  // second: let PE in getData, it will get data from FIFO
  poke(c.io.stateSW,1)
  step(1)
  step(1)
  step(1)
  poke(c.io.stateSW,2)
  step(1)
  step(1)
  for(i <- Range(0,50)){
    step(1)
  }
}

class PETester extends ChiselFlatSpec{

  "PEArray test use Basic " should "i dont know why to show this information" in {
    Driver(() => new PE(8,8,8)) {
      c => new PETest(c)
    }
    import fun._
    guoying()
  }

  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PE_vcd", "--top-name", "make_PE_vcd" ,
      "--backend-name", "verilator"),
      () => new PE(32, 32, 16)
    ) {
      c => new PETest(c)
    } should be(true)

    new File("test_run_dir/make_a_vcd/make_a_vcd.vcd").exists should be (true)

  }
}


class PETopTesterTester extends ChiselFlatSpec{

  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PETOP_vcd", "--top-name", "make_PETOP_vcd" ,
        "--backend-name", "verilator"),
      () => new PETesterTop
    ) {
      c => new PETopTester(c)
    } should be(true)

    new File("test_run_dir/make_PETOP_vcd/PETesterTop.vcd").exists should be (true)

  }
}

