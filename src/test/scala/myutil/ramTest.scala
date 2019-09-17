package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class ramTest(c:RAM) extends PeekPokeTester(c){
  def read(addr:UInt) = {
    poke(c.io.raddr, addr)
    poke(c.io.we, false)
//    peek(c.io.dout).toList
  }

  def write(addr:UInt, data:Int): Unit ={
    poke(c.io.waddr, addr)
    poke(c.io.we, true)
//    poke(c.io.din, IndexedSeq.fill(35)(data))
    poke(c.io.din, Array.fill(35)(BigInt(data)))
  }

  poke(c.io.we, 0)
  poke(c.io.waddr, 1)
  poke(c.io.raddr, 1)
  poke(c.io.din, Array.fill[BigInt](35)(1))
  step(1)
  step(1)
  write(1.asUInt(), 3)
  step(1)
  step(1)
  read(1.asUInt())
  step(1)
  step(1)
}

class MemChiselTester(c:MemChisel) extends PeekPokeTester(c){
  step(10)
  poke(c.io.raddr, 1)
  poke(c.io.we, 0)
  step(10)
}

class ramTester extends ChiselFlatSpec{
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_MemChisel_vcd",
        "--top-name", "make_MemChisel_vcd", "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new RAM(20, 280)
    ) {
      c => new ramTest(c)
    } should be(true)
  }
}
