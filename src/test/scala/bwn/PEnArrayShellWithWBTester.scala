// test for PEnArrayShell with WB
package bwn

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import config._
import node._

class PEnArrayShellWithWBTestTopTests(c: PEnArrayShellWithWBTestTop)(implicit p: Parameters) extends PeekPokeTester(c){
  val singleFilterLen = 3
  val imgNum = 1
  val relu = 1

  val filterNum = 1       //output channel
  val totalOutChannel = 1

  val nchannel = 1        //input Channel Group
  val singleImgLen = 8

  val filterAddr =  0x003f
  val filterStep =  0x0009

  val featureAddr = 0x0120
  val featureStep = 0x0020

  def go(filterAddr: BigInt, featureAddr: BigInt, forceOut: Int = 0, topOrBottom:Int = 1, replace:Int = 1): Unit ={
    // config PEConfig
    poke(c.io.peconfig.filterNum, filterNum)
    poke(c.io.peconfig.singleFilterLen, singleFilterLen)
    poke(c.io.peconfig.imgNum, imgNum)
    poke(c.io.peconfig.singleImgLen, singleImgLen)
    poke(c.io.peconfig.nchannel, nchannel)
    poke(c.io.peconfig.relu, relu)
    poke(c.io.peconfig.bias, 0)
    poke(c.io.peconfig.accState, 0)
    poke(c.io.peconfig.filterAddr, filterAddr)
    poke(c.io.peconfig.imgAddr, featureAddr)
    poke(c.io.peconfig.forceOut, forceOut)
    poke(c.io.peconfig.topOrBottom, topOrBottom)
    poke(c.io.peconfig.replace, replace)

    // stateSW 00 to 01
    poke(c.io.stateSW, 0)
    step(1)
    poke(c.io.stateSW, 1)
    step(1)
    poke(c.io.go, 1)
    step(1)
    poke(c.io.go, 0)
    poke(c.io.stateSW, 0)
    step(1)

    while (peek(c.io.done) != 1){
      step(1)
    }
    step(10)
  }

  go(0x00, 0x00, 0, 0, 1)
  go(0x09, 0x00, 0, 0, 1)
  go(0x12, 0x00, 0, 0, 1)
  go(0x1b, 0x00, 0, 0, 1)
  go(0x24, 0x00, 0, 0, 1)
  go(0x2d, 0x00, 0, 0, 1)
  go(0x36, 0x00, 0, 0, 1)
  go(0x3f, 0x00, 0, 0, 0)

  go(0x00, 0x08, 0, 1, 1)
  go(0x09, 0x08, 0, 1, 1)
  go(0x12, 0x08, 0, 1, 1)
  go(0x1b, 0x08, 0, 1, 1)
  go(0x24, 0x08, 0, 1, 1)
  go(0x2d, 0x08, 0, 1, 1)
  go(0x36, 0x08, 0, 1, 1)
  go(0x3f, 0x08, 0, 1, 0)


  go(0x00, 0x08, 0, 2, 1)
  go(0x09, 0x08, 0, 2, 1)
  go(0x12, 0x08, 0, 2, 1)
  go(0x1b, 0x08, 0, 2, 1)
  go(0x24, 0x08, 0, 2, 1)
  go(0x2d, 0x08, 0, 2, 1)
  go(0x36, 0x08, 0, 2, 1)
  go(0x3f, 0x08, 1, 2, 0)
//  for(featureaddr <- 0 to featureAddr by featureStep){
//    for(filteraddr <- 0 to filterAddr by filterStep){
//      println("filter : " + filteraddr.toString + " <-> " + "feature : " + featureaddr.toString)
//      if ((featureaddr == featureAddr) && (filteraddr == filterAddr)){
//        go(filteraddr, featureaddr, 1)
//        println(">>>>>>>>>>>>>>>   This is END")
//      }else{
//        go(filteraddr, featureaddr, 0)
//      }
//    }
//  }
  step(100)
}

class PEnArrayShellWithWBTester extends ChiselFlatSpec {
  implicit val p = new SmallWidthConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_PEnArrayShellWithWBTestTop_vcd",
        "--backend-name", "verilator",
        "--top-name", "make_PEnArrayShellWithWBTestTop_vcd"
      ),
      () => new PEnArrayShellWithWBTestTop
    ) {
      c => new PEnArrayShellWithWBTestTopTests(c)
    } should be(true)
  }
}
