package pe

import java.io.File

import breeze.linalg._
import breeze.linalg.{DenseMatrix, DenseVector}
import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import simulator._

object getVerilog extends App {
  println("generate pe.PE verilog")
  //  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PE)
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PETesterTop)
  //  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new test)
}


class PETest(c: PE) extends PeekPokeTester(c) {

  val a = RegInit(3.U(8.W))

  poke(c.io.stateSW, 0)
  step(1)
  step(1)
  step(1)
  step(1)
  poke(c.io.stateSW, 1)
  step(1)
  poke(c.io.filter.valid, 1)
  poke(c.io.filter.bits, 11)
  poke(c.io.img.valid, 1)
  poke(c.io.img.bits, 2)

  step(1)
  poke(c.io.img.bits, 3)
  poke(c.io.filter.bits, 4)
  step(1)
  poke(c.io.img.bits, 5)
  poke(c.io.filter.bits, 6)

  step(1)
  poke(c.io.img.bits, 7)
  poke(c.io.filter.bits, 8)

  step(1)
  poke(c.io.img.bits, 5)
  poke(c.io.filter.bits, 6)

  step(1)
  poke(c.io.stateSW, 2)
  step(1)
  step(1)
  poke(c.io.filter.valid, 0)
  poke(c.io.oSum.ready, 1)


  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
}

class PETopTester(c: PETesterTop) extends PeekPokeTester(c) {
  poke(c.io.stateSW, 0)
  step(1)
  poke(c.io.peconfig.filterNum, 4)
  poke(c.io.peconfig.singleFilterLen, 2)
  poke(c.io.peconfig.imgNum, 1)
  poke(c.io.peconfig.singleImgLen, 5)
  poke(c.io.peconfig.nchannel, 1)
  step(1)
  // first: let PE in idle and input data to input FIFO
  for (i <- Range(1, 11)) {
    poke(c.io.fIn.valid, 1)
    poke(c.io.iIn.valid, 1)
    poke(c.io.fIn.bits, i)
    poke(c.io.iIn.bits, i)
    step(1)
  }
  poke(c.io.fIn.valid, 0)
  poke(c.io.iIn.valid, 0)
  step(1)
  // second: let PE in getData, it will get data from FIFO
  poke(c.io.stateSW, 1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
  poke(c.io.stateSW, 2)
  step(1)
  step(1)
  for (i <- Range(0, 200)) {
    step(1)
  }
}

class testModen(c: PETesterTop, filter: List[Int], filterNum: Int, img: List[Int], imgNum: Int, nchannel: Int)
  extends PeekPokeTester(c) {
  val pe = new simulator.PE()
  pe.set(
    DenseVector(filter.toArray), filterNum,
    DenseVector(img.toArray), imgNum,
    nchannel
  )
  val sw = pe.cal
  //  println(pe.cal)
  // first let PE in idle state
  poke(c.io.stateSW, 0)
  step(1) // because PE buf state, so need 1 clock

  // second send basic infotmation to PE, include filterNum, singleFilterLen, imgNum, singleImgLen, nchannel
  poke(c.io.peconfig.filterNum, filterNum)
  poke(c.io.peconfig.singleFilterLen, filter.length / filterNum)
  poke(c.io.peconfig.imgNum, imgNum)
  poke(c.io.peconfig.singleImgLen, img.length / imgNum)
  poke(c.io.peconfig.nchannel, nchannel)
  step(1) // PE buf basic infotmation after 1 clock

  // third put data in
  poke(c.io.fIn.valid, 0)
  poke(c.io.iIn.valid, 0)
  filter.foreach((num) => {
    poke(c.io.fIn.valid, 1);
    poke(c.io.fIn.bits, num);
    step(1)
  })
  poke(c.io.fIn.valid, 0)
  img.foreach((num) => {
    poke(c.io.iIn.valid, 1);
    poke(c.io.iIn.bits, num);
    step(1)
  })
  poke(c.io.iIn.valid, 0)
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
    if (peek(c.io.oSum.valid) == 1) {
      //      println(peek(c.io.oSum.bits).toString())
      expect(c.io.oSum.bits, sw(j % filterNum, j / filterNum))
      //      println(s"${j} test pass")
      j = j + 1
    }
    step(1)
  }
  step(1)
  println("j: " + j.toString)
}

class PETester extends ChiselFlatSpec {

  "PEArray test use Basic " should "i dont know why to show this information" in {
    Driver(() => new PE(8, 8, 8)) {
      c => new PETest(c)
    }
    import fun._
    guoying()
  }

  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PE_vcd", "--top-name", "make_PE_vcd",
        "--backend-name", "verilator"),
      () => new PE(32, 32, 16)
    ) {
      c => new PETest(c)
    } should be(true)

    new File("test_run_dir/make_a_vcd/make_a_vcd.vcd").exists should be(true)

  }
}


class PETopTesterTester extends ChiselFlatSpec {

  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PETOP_vcd", "--top-name", "make_PETOP_vcd",
        "--backend-name", "verilator"),
      () => new PETesterTop
    ) {
      c => new PETopTester(c)
    } should be(true)
    new File("test_run_dir/make_PETOP_vcd/PETesterTop.vcd").exists should be(true)
  }
}

class PETopModeTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    var filter = List(1, 2, 3)
    var img = List(1, 2, 3, 4, 5)
    var filterNum = 1
    var imgNum = 1
    var nchannel = 1
    val random = scala.util.Random
    var mode = random.nextInt(1) + 1
    mode match {
      case 0 => {
        var fNum = random.nextInt(1) + 1
        var iNum = random.nextInt(1) + 1
        var fLen = random.nextInt(5) + 1
        var iLen = random.nextInt(64) + fLen
        filter = List.fill[Int](fLen)(random.nextInt(10))
        img = List.fill[Int](iLen)(random.nextInt(10))
        filterNum = fNum
        imgNum = iNum
        nchannel = 1
      }
      case 1 => {
        var fNum = random.nextInt(64) + 1
        var iNum = random.nextInt(1) + 1
        var fLen = random.nextInt(5) + 1
        var iLen = random.nextInt(64) + fLen
        var filter2d = DenseMatrix.fill[Int](fNum, fLen)(random.nextInt(10))
        filter = filter2d.toArray.toList
        img = List.fill[Int](iLen)(random.nextInt(10))
        filterNum = fNum
        imgNum = iNum
        nchannel = 1

//         var fNum = 4
//         var iNum = 1
//         var fLen = 3
//         var iLen = 6
//         filter = List(1,2,3,4,5,6,7,8,9,10,11,12)
//         img = List(1,2,3,4,5,6)
//         filterNum = fNum
//         imgNum = iNum
//         nchannel = 1
      }
      case _ => {}
    }

    val pe = new simulator.PE()
    pe.set(
      DenseVector(filter.toArray), filterNum,
      DenseVector(img.toArray), imgNum,
      nchannel
    )
    println(s"SW: ${pe.cal}")

    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_PETOPmode0_vcd", "--top-name", "make_PETOPmode0_vcd",
        "--backend-name", "verilator"),
      () => new PETesterTop
    ) {
      c => new testModen(c, filter, filterNum, img, imgNum, nchannel)
    } should be(true)
    new File("test_run_dir/make_PETOPmode0_vcd/PETesterTop.vcd").exists should be(true)

  }
}
