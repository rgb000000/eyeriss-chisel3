package node

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg._
import java.io.File
import simulator._

object getVerilog extends App {
  println("generate node verilog")
  //  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new Node(true, (1,1), 16))
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PEArray((6, 7)))
}

class PEArrayTest(c: PEArray, filter: DenseMatrix[DenseMatrix[Int]], img: DenseMatrix[DenseMatrix[Int]])
  extends PeekPokeTester(c) {
  val filterNum = filter.cols
  val fLen = filter(0, 0).cols
  val imgNum = img.cols
  val iLen = img(0, 0).cols
  val nchannel = filter.rows

  val sw = SW.conv4d(filter, img)
  println(s"filter: \n${filter}")
  println(s"img: \n${img}")
  val filter2d = SW.fd2List(filter, 0)
  val img2d = SW.fd2List(img, 1)
  println(s"filter2d: \n${filter2d}")
  println(s"img2d: \n${img2d}")
  println(s"SW: \n${sw}")


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
  for (i <- filter2d.indices) {               // row = filter's row
    for (j <- Range(0, iLen - fLen + 1)) {    // col = iLen - fLen + 1
      for (k <- img2d(0).indices) {
        poke(c.io.dataIn.valid, 1)
        poke(c.io.dataIn.bits.data, img2d(i + j)(k))
        poke(c.io.dataIn.bits.dataType, 1)
        poke(c.io.dataIn.bits.positon.row, i)
        poke(c.io.dataIn.bits.positon.col, j + 1)   //because cols 0  is  row controller
        step(1)
      }
    }
  }
  poke(c.io.dataIn.valid, 0)
  step(1)
  // fourth let PE in getdata state
  poke(c.io.stateSW, 1)
  println(s"filter2d(0) len : ${filter2d(0).length}")
  step(filter2d(0).length + 1)

  // finial let PE in cal state
  poke(c.io.stateSW, 2)
  step(1)
  c.io.oSum.foreach((x) => {
    poke(x.ready, 1)
  })
  var j = 0
  for (i <- Range(0, 20000)) {
    step(1)
  }
  step(1)
  println("j: " + j.toString)
}

class PEArrayTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    var filter4d = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
    var img4d = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
    var filterNum = 1
    var imgNum = 1
    var nchannel = 1
    var mode = 1
    var maxLen = 0
    mode match {
      case 1 => {
        do {
          val random = scala.util.Random
          nchannel = random.nextInt(1) + 1
          var fNum = random.nextInt(1) + 1
          var iNum = random.nextInt(1) + 1
          var fLen = random.nextInt(3) + 1
          var iLen = random.nextInt(3) + fLen + 1
          filter4d = DenseMatrix.fill(nchannel, fNum)(SW.randomMatrix((fLen, fLen)))
          img4d = DenseMatrix.fill(nchannel, iNum)(SW.randomMatrix((iLen, iLen)))
          filterNum = fNum
          imgNum = iNum
          maxLen = if (filterNum * fLen * nchannel > imgNum * iLen * nchannel) {
            filterNum * fLen * nchannel
          } else {
            imgNum * iLen * nchannel
          }
        } while (maxLen > 255)
      }
      case _ => {}
    }

    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PEArray_vcd", "--top-name", "make_PEArray_vcd",
        "--backend-name", "verilator"),
      () => new PEArray((6, 7))
    ) {
      c => new PEArrayTest(c, filter4d, img4d)
    } should be(true)
    new File("test_run_dir/make_PEArray_vcd/PEArray.vcd").exists should be(true)

  }
}
