// PEnArray Tester
package bwn

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import node.{PEArray2}
import config._
import node._

class PEnArrayTests(c: PEnArray) extends PeekPokeTester(c) {
  val filterNum = 1
  val singleFilterLen = 3
  val imgNum = 1
  val singleImgLen = 5
  val nchannel = 1
  val relu = 1

  def pokeFilter(bits: List[Int], id: Int): Unit = {
    bits.foreach((bit) => {
      poke(c.io.Freader.bits, bit)
      poke(c.io.Freader.valid, 1)
      poke(c.io.Fid, 1 << (id - 1))
      step(1)
      poke(c.io.Freader.valid, 0)
      poke(c.io.Fid, 0)
    })
  }

  def pokeImg(bitss: List[List[Int]]): Unit = {
    val row = bitss.length
    val col = bitss.head.length
    for(i <- 0 until col){
      (c.io.Ireaders.reverse, bitss.map(_(i))).zipped.foreach((io, data) => {
        poke(io.bits, data)
        poke(io.valid, 1)
      })
      step(1)
    }
    c.io.Ireaders.foreach((io) => {
      poke(io.valid, 0)
    })
  }

  // config PEConfig
  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, singleFilterLen)
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, singleImgLen)
  poke(c.io.peconfig.nchannel, nchannel)
  poke(c.io.peconfig.relu, relu)
//  poke(c.io.Write.ready, 1)

  // stateSW 00 to 01
  poke(c.io.stateSW, 0)
  step(1)
  poke(c.io.stateSW, 1)

  // filter
  pokeFilter(List(1, 1, 1), 3)
  pokeFilter(List(1, 1, 1), 2)
  pokeFilter(List(1, 1, 1), 1)
  // img
  pokeImg(List(
    List(1, 1, 1, 1, 1),
    List(2, 2, 2, 2, 2),
    List(3, 3, 3, 3, 3),
    List(4, 4, 4, 4, 4),
    List(5, 5, 5, 5, 5)))
  //  step(5)
  //  // img
  //  c.io.Ireaders.reverse.foreach((i)=>{
  //    poke(i.valid, 1)
  //    poke(i.bits, 1)
  //    step(5)
  //  })
  step(100)
  // bias
}

class PEnArrayShellTestTopTests(c: PEnArrayShellTestTop) extends PeekPokeTester(c) {
  val filterNum = 1
  val singleFilterLen = 3
  val imgNum = 1
  val nchannel = 1
  val relu = 1

  val singleImgLen = 5
  val totalOutChannel = 1

  // config PEConfig
  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, singleFilterLen)
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, singleImgLen)
  poke(c.io.peconfig.nchannel, nchannel)
  poke(c.io.peconfig.relu, relu)
  poke(c.io.peconfig.totalOutChannel, totalOutChannel)
  poke(c.io.peconfig.bias, 0)
  poke(c.io.peconfig.accState, 0)

  // stateSW 00 to 01
  poke(c.io.stateSW, 0)
  step(1)
  poke(c.io.stateSW, 1)
  step(1)
  poke(c.io.go, 1)
  step(1)

  while (peek(c.io.done) != 1){
    step(1)
  }

  poke(c.io.peconfig.accState, 1)
  step(50)

//  step(100)
}

class PEnArrayTester extends ChiselFlatSpec {
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_PEnArray_vcd",
        "--backend-name", "verilator",
        "--top-name", "make_PEnArray_vcd"
      ),
      () => new PEnArray
    ) {
      c => new PEnArrayTests(c)
    } should be(true)
  }
}

class PEnArrayShellTestTopTester extends ChiselFlatSpec {
  implicit val p = new DefaultConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_PEnArrayShellTestTop_vcd",
        "--backend-name", "verilator",
        "--top-name", "make_PEnArrayShellTestTop_vcd"
      ),
      () => new PEnArrayShellTestTop
    ) {
      c => new PEnArrayShellTestTopTests(c)
    } should be(true)
  }
}
