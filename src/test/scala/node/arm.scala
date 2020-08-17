// arm test
package node

import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, PeekPokeTester}
import config._

import scala.collection.mutable._

object cvt{
  def apply(x: Int):Int = {
    if (x > 0x80){
      x - 0x100
    }else{
      x
    }
  }
}

class PEArrayARMTests(c: PEArray_arm) extends PeekPokeTester(c) {

  import scala.io.Source

  // read weight
  val w = Source.fromFile("/home/l-black/Projects/mnist/conv1w.mem")
  val lines = w.getLines().toArray
  w.close()
  val weight: ListBuffer[List[Int]] = ListBuffer()
  for (line <- lines){
    val charList = line.grouped(2).toList.reverse
    val numList = charList.map((x) => Integer.parseInt(x, 16)).map(cvt(_))
    weight.append(numList)
  }

  // read picture
  val p = Source.fromFile("/home/l-black/Projects/mnist/img.mem")
  val plines = p.getLines().toArray
  p.close()
  val pic: ListBuffer[List[Int]] = ListBuffer()
  for (line <- plines){
    val charList = line.grouped(2).toList.reverse
    val numList = charList.map((x) => Integer.parseInt(x, 16)).map(cvt(_))
    pic.append(numList)
  }

  // read fc
  val fc_name = "/home/l-black/Projects/mnist/fc1_"
  val fc: ListBuffer[ListBuffer[List[Int]]] = ListBuffer()
  for(i <- 0 until 10){
    val fc_path = fc_name + i.toString + ".mem"
    val data = Source.fromFile(fc_path)
    val lines = data.getLines().toArray
    data.close()
    val fc_local: ListBuffer[List[Int]] = ListBuffer()
    for(line <- lines){
      val charList = line.grouped(2).toList.reverse
      val numList = charList.map((x) => Integer.parseInt(x, 16)).map(cvt(_))
      fc_local.append(numList)
    }
    fc.append(fc_local)
  }

  val filterNum = 5
  val singleFilterLen = 5
  val imgNum = 1
  val singleImgLen = 28
  val nchannel = 1
  val relu = 1
  val totalOutChannel = 5
  val bias = 0
  val accState = 0
  val filterAddr = 0
  val imgAddr = 0
  val forceOut = 0
  val topOrBottom = 0
  val replace = 0

  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, singleFilterLen)
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, singleImgLen)
  poke(c.io.peconfig.nchannel, nchannel)
  poke(c.io.peconfig.relu, relu)
  poke(c.io.peconfig.bias, 0)
  poke(c.io.peconfig.accState, 0)
  poke(c.io.peconfig.filterAddr, filterAddr)
  poke(c.io.peconfig.imgAddr, imgAddr)
  poke(c.io.peconfig.forceOut, forceOut)
  poke(c.io.peconfig.topOrBottom, topOrBottom)
  poke(c.io.peconfig.replace, replace)

  poke(c.io.stateSW, 0)
  step(1)
  poke(c.io.stateSW, 1)
  step(1)
  poke(c.io.stateSW, 0)
  poke(c.io.sumOut.ready, 1)
  step(1)

  var weight_i = 0
  var pic_i = 0
  var fc_i: ArrayBuffer[Int] = ArrayBuffer.fill(10)(0)
  var cycle = 0
  while (cycle < 2000){
    if ((peek(c.io.filterIn.ready) == 1) & (weight_i < 25)){
      poke(c.io.filterIn.valid, 1)
      (c.io.filterIn.bits, weight.map(_ (weight_i))).zipped.foreach(poke(_, _))
      weight_i += 1
    }
    if((peek(c.io.featureIn.ready) == 1) & (pic_i < 28)){
      poke(c.io.featureIn.valid, 1)
      (c.io.featureIn.bits, pic.map(_ (pic_i))).zipped.foreach(poke(_, _))
      pic_i += 1
    }

    for(i <- 0 until 10){
      if((peek(c.io.inB(i).ready) == 1) & (fc_i(i) < 60)){
        println("give a B")
        poke(c.io.inB(i).valid, 1)
        (c.io.inB(i).bits, fc(i)(fc_i(i))).zipped.foreach(poke(_, _))
        fc_i(i) += 1
      }
    }



    step(1)
    cycle += 1
    poke(c.io.filterIn.valid, 0)
    poke(c.io.featureIn.valid, 0)
    c.io.inB.foreach((x) => {
      poke(x.valid, 0)
    })
  }
}

class PEArrayARMTester extends ChiselFlatSpec {
  implicit val p = new ArmConfig
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array(
        "--generate-vcd-output", "on",
        "--target-dir", "test_run_dir/make_arm_vcd",
        "--backend-name", "verilator",
        "--top-name", "make_arm_vcd"
      ),
      () => new PEArray_arm
    ) {
      c => new PEArrayARMTests(c)
    } should be(true)
  }
}

object readFile extends App {

  import scala.io.Source

//  // read weight
//  val w = Source.fromFile("/home/l-black/Projects/mnist/conv1w.mem")
//  val lines = w.getLines().toArray
//  w.close()
//  val weight: ListBuffer[List[Int]] = ListBuffer()
//  for (line <- lines){
//    val charList = line.grouped(2).toList.reverse
//    val numList = charList.map((x) => Integer.parseInt(x, 16)).map(cvt(_))
//    weight.append(numList)
//  }
//
//  // read picture
//  val p = Source.fromFile("/home/l-black/Projects/mnist/img.mem")
//  val plines = p.getLines().toArray
//  p.close()
//  val pic: ListBuffer[List[Int]] = ListBuffer()
//  for (line <- plines){
//    val charList = line.grouped(2).toList.reverse
//    val numList = charList.map((x) => Integer.parseInt(x, 16)).map(cvt(_))
//    pic.append(numList)
//  }

  // read fc
  val fc_name = "/home/l-black/Projects/mnist/fc1_"
  val fc: ListBuffer[ListBuffer[List[Int]]] = ListBuffer()
  for(i <- 0 until 10){
    val fc_path = fc_name + i.toString + ".mem"
    val data = Source.fromFile(fc_path)
    val lines = data.getLines().toArray
    data.close()
    val fc_local: ListBuffer[List[Int]] = ListBuffer()
    for(line <- lines){
      val charList = line.grouped(2).toList.reverse
      val numList = charList.map((x) => Integer.parseInt(x, 16)).map(cvt(_))
      fc_local.append(numList)
    }
    fc.append(fc_local)
  }
  println(fc)
}

