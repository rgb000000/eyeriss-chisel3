package node

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg._
import simulator._

class Fudan(c: PEArray) extends PeekPokeTester(c) {

  def saturationSW(x: Int): Int = {
    val tmp = if (x >= 0) {
      x / 4.0 + 0.5
    } else {
      x / 4.0 - 0.5
    }
    if (tmp >= 127) {
      127
    } else if (tmp <= -128) {
      -128
    } else {
      tmp.toInt
    }
  }

  var filter = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
  var img = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
  var filterNum = 1
  var imgNum = 1
  var nchannel = 64
  var fLen = 3
  var iLen = 34 // padding = 1
  var maxLen = 0
  var bias = 0
  filter = DenseMatrix.fill(nchannel, filterNum)(SW.randomMatrix((fLen, fLen)))
  img = DenseMatrix.fill(nchannel, imgNum)(SW.randomMatrix((iLen, iLen)))
  maxLen = if (filterNum * fLen * nchannel > imgNum * iLen * nchannel) {
    filterNum * fLen * nchannel
  } else {
    imgNum * iLen * nchannel
  }

  var sw1 = SW.conv4d(filter, img, true, bias=DenseMatrix.fill[Int](1, filter.cols )(bias))
  val filter2d = SW.fd2List(filter, 0)
  val img2d = SW.fd2List(img, 1)

  sw1.map((x)=>{
    print(x.toString())
    println()
  })
  println("sw: ")
  val sw = sw1.map((x) => {
    x.map((num)=>{saturationSW(num)})
  })
  sw.map((x)=>{
    print(x.toString())
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

  poke(c.io.stateSW, 0)
  poke(c.io.bias, bias)
  step(100) // because PE buf state, so need 1 clock

  // second send basic infotmation to PE, include filterNum, singleFilterLen, imgNum, singleImgLen, nchannel
  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, fLen)
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, iLen)
  poke(c.io.peconfig.nchannel, nchannel)
  poke(c.io.peconfig.relu, 1)
  step(1) // PE buf basic infotmation after 1 clock

  poke(c.io.stateSW, 1)

  // third put data in
  poke(c.io.dataIn.valid, 0)
  for (i <- filter2d.indices) {
    for (j <- filter2d(0).indices) {
      poke(c.io.dataIn.valid, 1)
      poke(c.io.dataIn.bits.data(0), filter2d(i)(j))
      poke(c.io.dataIn.bits.dataType, 0)
      poke(c.io.dataIn.bits.positon.row, i)
      poke(c.io.dataIn.bits.positon.col, -1)
      poke(c.io.dataIn.bits.cnt, 1)
      step(1)
    }
  }

  val img2d_group = img2d.map(_.grouped(iLen).toList)
  for (i <- img2d_group(0).indices) {
    for (j <- img2d_group.indices) {
      for (k <- img2d_group(j)(i).indices) {
        poke(c.io.dataIn.bits.data(k), img2d_group(j)(i)(k))
      }
      poke(c.io.dataIn.valid, 1)
      poke(c.io.dataIn.bits.dataType, 1)
      poke(c.io.dataIn.bits.positon.row, j)
      poke(c.io.dataIn.bits.positon.col, 1) //because cols 0  is  row controller
      poke(c.io.dataIn.bits.cnt, img2d_group(j)(i).length)
      step(1)
      if(peek(c.io.dataDone) == 1){
        poke(c.io.stateSW, 2)
      }
    }
  }

  poke(c.io.dataIn.valid, 0)
  step(1)
  // fourth let PE in getdata state
  //    println(s"filter2d(0) len : ${filter2d(0).length}")
  //  step(filter2d(0).length + 1)
  // finial let PE in cal state
  //    c.io.oSumMEM.foreach((x) => {
  //      poke(x.ready, 1)
  //    })
  c.io.oSumSRAM.foreach((x) => {
    poke(x.ready, 1)
  })
  var error = 0
  var jj = List.fill(c.io.oSumSRAM.length)(0).toBuffer
  var row = 0
  while (peek(c.io.done) == 0) {
    row += 1
    for (i <- c.io.oSumSRAM.indices) {
      if (peek(c.io.oSumSRAM(i).valid) == 1) {
        expect(c.io.oSumSRAM(i).bits, sw1d(i * singLen + jj(i)))
        //          expect(c.io.oSumMEM(i).bits, sw1d(i * singLen + jj(i)))
        // println(peek(c.io.oSum(i).bits).toString())
        if (peek(c.io.oSumSRAM(i).bits) != sw1d(i * singLen + jj(i))) {
          println(s"${row} - ${i} : "  + peek(c.io.oSumSRAM(i).bits).toString() + " --- " + sw1d(i * singLen + jj(i)).toString)
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
  //  assert(jj.reduce(_ + _) == sw1d.length)
  reset(50)
  println(s"===============ERROR: ${error}======================")

}

class FudanTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_fudan_vcd", "--backend-name", "verilator",
        "--top-name", "make_fudan_vcd"),
      () => new PEArray((3, 32))
    ) {
      c => new Fudan(c)
    } should be(true)
    //    new File("test_run_dir/make_PEArray_vcd/PEArray.vcd").exists should be(true)
  }
}
