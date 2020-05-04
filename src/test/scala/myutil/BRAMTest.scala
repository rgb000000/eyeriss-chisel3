package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import config.DefaultConfig


class BRAMFilterReaderTests(c:BRAMFilterReaderTestTop) extends PeekPokeTester(c){
  poke(c.io.len, 8*9)
  poke(c.io.inChannelGroup, 8)
  poke(c.io.addr, 0)
  poke(c.io.go, 0)
  step(1)
  poke(c.io.go, 1)
  step(1)
  step(10)
  poke(c.io.dout.ready, 1)
  step(10)
  poke(c.io.dout.ready, 0)
  step(10)
  poke(c.io.dout.ready, 1)
  step(500)
}

class BRAMImgReaderTests(c:BRAMImgReaderTestTop) extends PeekPokeTester(c){
  val len = 32
  val inChannel = 1
  def go(addr: BigInt, topOrBotton: Int, replace: Int = 0): Unit ={
    reset(1)
    poke(c.io.len, len)
    poke(c.io.addr, addr)
    poke(c.io.topOrBottom, topOrBotton)
    poke(c.io.inChannel, inChannel)
    poke(c.io.go, 0)
    poke(c.io.replace, replace)
    poke(c.io.doutSplit.ready, 1)
    step(1)
    poke(c.io.go, 1)
    step(1)
    poke(c.io.go, 0)
    while(peek(c.io.done) != 1){
      step(1)
    }
  }
  go(0x00, 0, 1)
  go(0x00, 0, 1)
  go(0x00, 0, 1)
  go(0x00, 0, 0)

  go(0x20, 1, 1)
  go(0x20, 1, 1)
  go(0x20, 1, 1)
  go(0x20, 1, 0)

  go(0x40, 1, 1)
  go(0x40, 1, 1)
  go(0x40, 1, 1)
  go(0x40, 1, 0)

  go(0x40, 2, 1)
  go(0x40, 2, 1)
  go(0x40, 2, 1)
  go(0x40, 2, 1)
//  go(0x40, 1)
//  go(0x60, 1)
//  go(0x80, 1)
//  go(0xa0, 1)
//  go(0xc0, 1)
//  go(0xe0, 1)
//  go(0xc0, 1)
  step(100)
}

class BRAMFilterReaderTester extends ChiselFlatSpec{
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_BRAMFilterTest_vcd",
        "--top-name", "make_BRAMFilterTest_vcd",
        "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new BRAMFilterReaderTestTop
    ) {
      c => new BRAMFilterReaderTests(c)
    } should be(true)
  }
}

class BRAMImgReaderTester extends ChiselFlatSpec{
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_BRAMImgTest_vcd",
        "--top-name", "make_BRAMImgTest_vcd",
        "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new BRAMImgReaderTestTop
    ) {
      c => new BRAMImgReaderTests(c)
    } should be(true)
  }
}
