package node

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg._
import java.io.File

import firrtl.annotations.TargetToken.Reset
import simulator._

object getVerilog extends App {
  println("generate node verilog")
  //  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new Node(true, (1,1), 16))
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PEArray((6, 7)))
}

class PEArrayTest(c: PEArray, /*filter:DenseMatrix[DenseMatrix[Int]],img:DenseMatrix[DenseMatrix[Int]],*/ var loop: Int)
  extends PeekPokeTester(c) {
  do {
    var filter = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
    var img = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
    var filterNum = 1
    var imgNum = 1
    var nchannel = 1
    var maxLen = 0
    var fLen = 0
    var iLen = 0
    do {
      val random = scala.util.Random
      nchannel = random.nextInt(32) + 1
      filterNum = random.nextInt(32) + 1
      imgNum = random.nextInt(32) + 1
      fLen = random.nextInt(6) + 1
      iLen = random.nextInt(16) +fLen + 1
      filter = DenseMatrix.fill(nchannel, filterNum)(SW.randomMatrix((fLen, fLen)))
      img = DenseMatrix.fill(nchannel, imgNum)(SW.randomMatrix((iLen, iLen)))
      maxLen = if (filterNum * fLen * nchannel > imgNum * iLen * nchannel) {
        filterNum * fLen * nchannel
      } else {
        imgNum * iLen * nchannel
      }
    } while (maxLen > 255 | iLen > 7 + fLen - 1  | fLen > 6)

    //    val filterNum = filter.cols
//    val fLen = filter(0, 0).cols
    //    val imgNum = img.cols
//    val iLen = img(0, 0).cols
    //    val nchannel = filter.rows

    val sw = SW.conv4d(filter, img)
    val filter2d = SW.fd2List(filter, 0)
    val img2d = SW.fd2List(img, 1)
    println(s"filterNum: ${filterNum}")
    println(s"imgNum: ${imgNum}")
    println(s"channel: ${nchannel}")
    println(s"fLen: ${fLen}")
    println(s"iLen: ${iLen}")
    filter2d.map((x) => {
      println(x.toString())
      println()
    })
    println("img2d: ")
    img2d.map((x) => {
      println(x.toString())
      println()
    })
        println("sw: ")
        sw.map((x) => {
          println(x.toString());
          println()
        })

    val sw1d = List[Int]().toBuffer
    val singLen = sw(0, 0).cols * sw.size
    for (k <- Range(0, sw(0, 0).rows)) {
      for (i <- Range(0, sw.cols)) {
        for (l <- Range(0, sw(0, 0).cols)) {
          for (j <- Range(0, sw.rows)) {
            sw1d.append(sw(j, i)(k, l))
          }
        }
      }
    }
//    println(sw1d.toString())


    poke(c.io.stateSW, 0)
    step(1) // because PE buf state, so need 1 clock

    // second send basic infotmation to PE, include filterNum, singleFilterLen, imgNum, singleImgLen, nchannel
    poke(c.io.peconfig.filterNum, filterNum)
    poke(c.io.peconfig.singleFilterLen, fLen)
    poke(c.io.peconfig.imgNum, imgNum)
    poke(c.io.peconfig.singleImgLen, iLen)
    poke(c.io.peconfig.nchannel, nchannel)
    step(1) // PE buf basic infotmation after 1 clock

    // third put data in
    poke(c.io.dataIn.valid, 0)
    for (i <- filter2d.indices) {
      for (j <- filter2d(0).indices) {
        poke(c.io.dataIn.valid, 1)
        poke(c.io.dataIn.bits.data, filter2d(i)(j))
        poke(c.io.dataIn.bits.dataType, 0)
        poke(c.io.dataIn.bits.positon.row, i)
        poke(c.io.dataIn.bits.positon.col, -1)
        step(1)
      }
    }
    for (i <- filter2d.indices) { // row = filter's row
      for (j <- Range(0, iLen - fLen + 1)) { // col = iLen - fLen + 1
        for (k <- img2d(0).indices) {
          poke(c.io.dataIn.valid, 1)
          poke(c.io.dataIn.bits.data, img2d(i + j)(k))
          poke(c.io.dataIn.bits.dataType, 1)
          poke(c.io.dataIn.bits.positon.row, i)
          poke(c.io.dataIn.bits.positon.col, j + 1) //because cols 0  is  row controller
          step(1)
        }
      }
    }
    poke(c.io.dataIn.valid, 0)
    step(1)
    // fourth let PE in getdata state
    poke(c.io.stateSW, 1)
//    println(s"filter2d(0) len : ${filter2d(0).length}")
    step(filter2d(0).length + 1)

    // finial let PE in cal state
    poke(c.io.stateSW, 2)
    step(1)
//    c.io.oSumMEM.foreach((x) => {
//      poke(x.ready, 1)
//    })
    c.io.oSumSRAM.foreach((x) => {
      poke(x.ready, 1)
    })
    var error = 0
    var jj = List.fill(c.io.oSumSRAM.length)(0).toBuffer
     while(peek(c.io.done) == 0){
      for (i <- c.io.oSumSRAM.indices) {
        if (peek(c.io.oSumSRAM(i).valid) == 1) {
          expect(c.io.oSumSRAM(i).bits, sw1d(i * singLen + jj(i)))
//          expect(c.io.oSumMEM(i).bits, sw1d(i * singLen + jj(i)))
          // println(peek(c.io.oSum(i).bits).toString())
          if(peek(c.io.oSumSRAM(i).bits) != sw1d(i * singLen + jj(i))){
            error += 1
          }
          jj(i) += 1
        }
      }
      step(1)
    }
    step(1)
    println(s"jj reduce: ${jj.reduce(_ + _)}")
    println(s"sw1d: ${sw1d.length}")
    assert(jj.reduce(_ + _) == sw1d.length)
    reset(50)
    loop -= 1
    println(s"loop: ${loop + 1}")
    println(s"===============ERROR: ${error}======================")
  } while (loop > 0)

}

class PEArrayTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "off", "--target-dir", "test_run_dir/make_PEArray_vcd", "--top-name", "make_PEArray_vcd",
        "--backend-name", "verilator"),
      () => new PEArray((6, 7))
    ) {
      c => new PEArrayTest(c, 100)
    } should be(true)
//    new File("test_run_dir/make_PEArray_vcd/PEArray.vcd").exists should be(true)

  }
}
