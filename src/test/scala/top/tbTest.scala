package top

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg._
import simulator._

class tbTest(c: TB, info: Map[String, Int], sw1d: List[List[Int]]) extends PeekPokeTester(c) {

  val filterNum = info("filterNum")
  val imgNum = info("imgNum")
  val nchannel = info("nchannel")
  val fLen = info("fLen")
  val iLen = info("iLen")
  val singLen = info("singLen")
  val loop = info("loop")

  val onceLen = singLen * (iLen - fLen + 1)/2

  def writeReg(addr:Int, data:Int): Unit ={
    poke(c.io.regfile.we, 1)
    poke(c.io.regfile.addr, addr)
    poke(c.io.regfile.din, data)
    step(1)
    poke(c.io.regfile.we, 0)
  }
  writeReg(0, filterNum)
  writeReg(1, fLen)
  writeReg(2, imgNum)
  writeReg(3, iLen)
  writeReg(4, nchannel)
  writeReg(5, 1)
  writeReg(6, loop)
//  poke(c.io.peconfig.filterNum, filterNum)
//  poke(c.io.peconfig.singleFilterLen, fLen)
//  poke(c.io.peconfig.imgNum, imgNum)
//  poke(c.io.peconfig.singleImgLen, iLen)
//  poke(c.io.peconfig.nchannel, nchannel)
//  poke(c.io.peconfig.relu, 1)
  step(1) // PE buf basic information after 1 clock
  writeReg(7, 1)
//  poke(c.io.readGo, 1)

  var error = 0
  var jj = List.fill(loop, c.io.wdata.length)(0).map(_.toBuffer).toBuffer
  var row = 0
  var times = 0
  while (peek(c.io.done) == 0) {
    for (i <- c.io.wdata.indices) {
      if ((peek(c.io.wdata_valid(i)) == 1))  {
        expect(c.io.wdata(i), sw1d(times)(i * singLen + jj(times)(i)))
        if (peek(c.io.wdata(i)) != sw1d(times)(i * singLen + jj(times)(i))) {
          println("index : " + times.toString + " -- " + (i * singLen + jj(times)(i)).toString)
          println(peek(c.io.wdata(i)).toString() + " --- " + sw1d(times)(i * singLen + jj(times)(i)).toString)
          error += 1
        }
        jj(times)(i) += 1
        if(jj.map(_.reduce(_+_)).reduce(_+_) % onceLen == 0){
          println("time: ===> " + times.toString)
          times += 1
        }
      }
    }
    step(1)
  }
  step(1)
  println(s"jj reduce: ${jj.map(_.reduce(_+_)).reduce(_ + _)}")
  println(s"sw1d: ${sw1d.map(_.length).reduce(_+_)}")
  //  assert(jj.reduce(_ + _) == sw1d.length)
  step(200)
  reset(50)
  println(s"===============ERROR: ${error}======================")
}

class tbTester extends ChiselFlatSpec {
  val filterNum = 4
  val imgNum = 1
  val nchannel = 64
  val fLen = 3
  val iLen = 34 // padding = 1
  val loop = 32
  val (myinfo, sw1d) = GenTestData(filterNum, imgNum, nchannel, fLen, iLen, loop)
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "off", "--target-dir", "test_run_dir/make_TB_vcd", "--backend-name", "verilator",
        "--top-name", "make_TB_vcd"),
      () => new TB(0x0000, 0x240*filterNum+filterNum*(1 + ( (loop-1)/16 )) )       ) {
      c => new tbTest(c, myinfo, sw1d)
    } should be(true)
    //    new File("test_run_dir/make_PEArray_vcd/PEArray.vcd").exists should be(true)
  }
}

object getTopVerilog extends App{
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_TOP770_verilog"), () => new Top(0x0000, 2312))
}

object getTopVerilog2 extends App{
  val filterNum = 4
  val imgNum = 1
  val nchannel = 64
  val fLen = 3
  val iLen = 34 // padding = 1
  val loop = 1
  val (myinfo, sw1d) = GenTestData(filterNum, imgNum, nchannel, fLen, iLen, loop)
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_TB_verilog"),() => new TB(0x0000, 0x240*filterNum+filterNum*(1 + ( (loop-1)/16 )) ) )
}
