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

class PEArrayTest(c: PEArray, filter: List[Int], filterNum: Int, img: List[Int], imgNum: Int, nchannel: Int)
  extends PeekPokeTester(c) {
  val swDM = SW.convGeneral(DenseVector(filter: _*), filterNum, DenseVector(img: _*), imgNum, nchannel)
  println("SW: \n" + swDM.toString())
  val sw = List[Int]().toBuffer
  for (i <- Range(0, imgNum)) {
    swDM(i * filterNum to (i + 1) * filterNum - 1, ::).flatten().toArray.toList.foreach(sw.append(_))
    //    println(swDM(i * filterNum to (i + 1) * filterNum - 1, ::).flatten().toArray.toList.toString())
  }
  poke(c.io.stateSW, 0)
  step(1) // because PE buf state, so need 1 clock

  // second send basic infotmation to PE, include filterNum, singleFilterLen, imgNum, singleImgLen, nchannel
  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, filter.length / (filterNum * nchannel))
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, img.length / (imgNum * nchannel))
  poke(c.io.peconfig.nchannel, nchannel)
  step(1) // PE buf basic infotmation after 1 clock

  // third put data in
  poke(c.io.dataIn.valid, 0)
  filter.foreach((num) => {
    poke(c.io.dataIn.valid, 1)
    poke(c.io.dataIn.bits.data, num)
    poke(c.io.dataIn.bits.dataType, 0)
    poke(c.io.dataIn.bits.positon.row, 3)
    poke(c.io.dataIn.bits.positon.col, -1)
    step(1)
  })

  img.foreach((num) => {
    poke(c.io.dataIn.valid, 1)
    poke(c.io.dataIn.bits.data, num)
    poke(c.io.dataIn.bits.dataType, 1)
    poke(c.io.dataIn.bits.positon.row, 3)
    poke(c.io.dataIn.bits.positon.col, -1)
    step(1)
  })
  poke(c.io.dataIn.valid, 0)
  step(1)

  // fourth let PE in getdata state
  poke(c.io.stateSW, 1)
  step(filter.length)

  // finial let PE in cal state
  poke(c.io.stateSW, 2)
  step(1)
  var j = 0
  for (i <- Range(0, 20000)) {
    //    println("oSum.bits: " + peek(c.io.oSum.valid).toString())
    //    println("sw.cols: " + sw.cols.toString())
    //    println("sw.rows: " + sw.rows.toString)
    //    if (peek(c.io.oSum.valid) == 1) {
    //      //      println(peek(c.io.oSum.bits).toString())
    //      expect(c.io.oSum.bits, sw(j))
    //      print(sw(j).toString + " <---> " + peek(c.io.oSum.bits).toString() + "   ")
    //      if(sw(j) == peek((c.io.oSum.bits))){
    //        println("pass")
    //      }else{
    //        println("FAID")
    //      }

    //      println(s"${j} test pass")
    j = j + 1
    step(1)
  }
  step(1)
  println("j: " + j.toString)
}

class PEArrayTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    var filter = List(1, 2, 3)
    var img = List(1, 2, 3, 4, 5)
    var filterNum = 1
    var imgNum = 1
    var nchannel = 1
    val random = scala.util.Random
    var mode = 1
    mode match {
      case 1 => {
        do {
          nchannel = random.nextInt(5) + 1
          var fNum = random.nextInt(32) + 1
          var iNum = random.nextInt(5) + 1
          var fLen = random.nextInt(3) + 1
          var iLen = random.nextInt(16) + fLen
          var filter2d = DenseMatrix.fill[Int](fNum * nchannel, fLen)(random.nextInt(10) - 5)
          var img2d = DenseMatrix.fill[Int](iNum * nchannel, iLen)(random.nextInt(10) - 5)
          filter = filter2d.toArray.toList
          img = img2d.toArray.toList
          filterNum = fNum
          imgNum = iNum
        } while (filter.length > 255 | img.length > 255)

        //        var fNum = 3
        //        var iNum = 3
        //        var fLen = 2
        //        var iLen = 3
        //        filter = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
        //        img = List(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6)
        //        filterNum = fNum
        //        imgNum = iNum
        //        nchannel = 2
      }
      case _ => {}
    }

    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PEArray_vcd", "--top-name", "make_PEArray_vcd",
        "--backend-name", "verilator"),
      () => new PEArray((6, 7))
    ) {
      c => new PEArrayTest(c, filter, filterNum, img, imgNum, nchannel)
    } should be(true)
    new File("test_run_dir/make_PETOPmode0_vcd/PETesterTop.vcd").exists should be(true)

  }
}
