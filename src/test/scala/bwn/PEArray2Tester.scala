// PEArray2 Tester

package bwn

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import node.{PEArray2}

class PEArray2Tests(c: PEArray2) extends PeekPokeTester(c){
  val filterNum = 3
  val singleFilterLen = 3
  val imgNum = 1
  val singleImgLen = 5
  val nchannel = 3
  val relu = 1

  val filter = List(
    List(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3),
    List(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3),
    List(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3),
  )
  val img = List(
    List(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
    List(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
    List(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
    List(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
    List(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5),
  )

  // config PEConfig
  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, singleFilterLen)
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, singleImgLen)
  poke(c.io.peconfig.nchannel, nchannel)
  poke(c.io.peconfig.relu, relu)

  // stateSW 00 to 01
  poke(c.io.stateSW, 0)
  step(1)
  poke(c.io.stateSW, 1)

  // filter
  for(i <- 0 until filter.head.length){
    (c.io.filter_in, filter.map(_(i))).zipped.foreach((in, data) => {
      poke(in.valid, 1)
      poke(in.bits, data)
      step(1)
      poke(in.valid, 0)
      step(1)
    })
  }
  // img
  for(i <- 0 until img.head.length){
    (c.io.img_in, img.map(_(i))).zipped.foreach((in, data) => {
      poke(in.valid, 1)
      poke(in.bits, data)
      step(1)
      poke(in.valid, 0)
      step(1)
    })
  }
  // bias
  poke(c.io.bias.valid, 1)
  poke(c.io.bias.bits, 1)
  // oSum
  c.io.oSum.foreach(in=>{
    poke(in.ready, 1)
  })

  while (peek(c.io.dataDone) != 1){
    step(1)
  }
  step(1)
  poke(c.io.stateSW, 2)
  step(100)
}

class PEArray2Tester extends ChiselFlatSpec {
  val shape = (3, 3)
  val w = 8
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_bwn_pearray2_vcd",
        "--backend-name", "verilator",
        "--top-name", "make_fudan_vcd"
      ),
      () => new PEArray2(shape, w)
    ) {
      c => new PEArray2Tests(c)
    } should be(true)
  }
}
