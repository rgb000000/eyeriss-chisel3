// Test Conv

package conv

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg._

import scala.collection.mutable.ArrayBuffer

object GetVerilog extends App {
  println("hi")
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PE(3, 5, 8))
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"),
    () => new PEArray(2, 2, 3, 3, 16))
}

object SW extends App {
  // PE.py 29~37 def __PE__
  // TEST PASS
  def conv1d(filter: List[Int], img: List[Int], sumIn: List[Int]): List[Int] = {
    var result: List[Int] = List()
    for (i <- 0 to img.length - filter.length) {
      var localSum: Int = 0
      for (j <- filter.indices) {
        localSum += filter(j) * img(j + i)
      }
      result = result :+ localSum
    }
    (result, sumIn).zipped.map(_ + _)
  }

  def conv2d(filter: List[List[Int]], img: List[List[Int]]): List[List[Int]] = {
    val fyLen = filter.length
    val fxLen = filter(0).length
    val ixLen = img(0).length
    val iyLen = img.length
    val result = ArrayBuffer.fill(iyLen - fyLen + 1, ixLen - fxLen + 1)(0)
    for (i <- 0 until iyLen - fyLen + 1) {
      for (j <- 0 until ixLen - fxLen + 1) {
        result(i)(j) = (filter, img.slice(i, i + fyLen)).zipped.map(
          (f: List[Int], img: List[Int]) => {
            (f, img.slice(j, j + fxLen)).zipped.map(_ * _).sum
          }
        ).sum
      }
    }
    result.map(_.toList).toList
  }

  // TODO: conInterleave filter and fmap
  def convInterleave(filterin: List[Int], fmapin: List[Int], filterNum: Int,
                     fmapNum: Int, nchannel: Int, model: Int): DenseMatrix[Int] = {

    def shiftReg(x: List[Int]): List[Int] = {
      return (x.tail :+ x.head)
    }

    var filterReg = filterin
    var fmapReg = fmapin.slice(0, filterReg.length)
    var pSumReg = DenseVector.fill[Int](100)(0)
    var oSumReg = DenseMatrix.fill[Int](filterNum, fmapin.length - filterin.length/filterNum + 1)(0)

    model match {
      case 0 => {
        // 1 filter  1 image  and 1 channel, reuse filter
        assert(filterNum == 1 & nchannel == 1 & fmapNum == 1)

        for (i <- 0 until fmapin.length - filterin.length + 1) {
          for (j <- filterReg.indices) {
            pSumReg(i) += filterReg.head * fmapReg.head
            filterReg = shiftReg(filterReg)
            fmapReg = shiftReg(fmapReg)
          }
          oSumReg(0, i) = pSumReg(i)
          fmapReg = fmapin.slice(i + 1, i + 1 + filterReg.length)
        }
        oSumReg
      }

      case 1 => {
        // several filters and one img, reuse img
        assert(fmapNum == 1 & nchannel == 1 & filterNum != 1)

        for (i <- 0 until fmapin.length - filterin.length / filterNum + 1) {
          for (j <- Range(0, filterReg.length / filterNum)) {
            for (k <- 0 until filterNum) {
              pSumReg(i * filterNum + k) += filterReg.head * fmapReg.head
              filterReg = shiftReg(filterReg)
            }
            fmapReg = shiftReg(fmapReg)
          }
          oSumReg(0 until filterNum, i) := pSumReg(i * filterNum until (i + 1) * filterNum)
          fmapReg = fmapin.slice(i + 1, i + 1 + filterReg.length)
        }
        oSumReg
      }
      case 2 => {
        // servel channel
        assert(nchannel != 1)
        var oSumReg = DenseMatrix.fill[Int](nchannel, fmapin.length/nchannel - filterin.length/nchannel + 1)(0)
        for (i <- 0 until fmapin.length/nchannel - filterin.length / nchannel + 1) {
          for (j <- Range(0, filterReg.length/nchannel)) {
            for (k <- 0 until nchannel) {
              pSumReg(i * nchannel + k) += filterReg.head * fmapReg.head
              filterReg = shiftReg(filterReg)
              fmapReg = shiftReg(fmapReg)
            }
          }
          oSumReg(0 until nchannel, i) := pSumReg(i * nchannel until (i + 1) * nchannel)
          fmapReg = fmapin.slice(i + nchannel, i + nchannel + filterReg.length)
        }
        oSumReg
      }
    }
  }

  def test(ss: String): Unit = {
    println(ss)
  }

  def testConv() = {
    val f = List(1, 2, 3, 1, 2, 3, 1, 2)
    val i = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 1, 1, 1, 2, 3, 4, 5, 5, 6)
    conv1d(f, i, List.fill(i.length - f.length + 1)(1)).foreach((x: Int) => print(x.toString() + ","))
    test("hi")

    val r = scala.util.Random
    val ff = List.fill(3, 3)(r.nextInt(10) - 5)
    val ii = List.fill(5, 5)(r.nextInt(10) - 5)
    println(ff)
    println(ii)
    println(conv2d(ff, ii))
  }

  def testconvInterleave(): Unit = {
    println(convInterleave(List(1, 2, 3, 4), List(1, 2, 3, 4, 5, 6), 1, 1, 2, 2))
    println(conv1d(List(1, 3), List(1, 3, 5), List(0, 0, 0, 0)))
    println(conv1d(List(2, 4), List(2, 4, 6), List(0, 0, 0, 0)))
  }

  testconvInterleave()
}


class PETest(c: PE) extends PeekPokeTester(c) {
  val r = scala.util.Random
  val f = List.fill(c.filterLen)(r.nextInt(10) - 5)
  val i = List.fill(c.imgLen)(r.nextInt(10) - 5)
  val sumIn = List.fill(c.imgLen - c.filterLen + 1)(r.nextInt(10) - 5)
  val sw = SW.conv1d(f, i, sumIn)
  (c.io.filterRow, f).zipped.map(poke(_, _))
  (c.io.imgRow, i).zipped.map(poke(_, _))
  (c.io.sumIn, sumIn).zipped.map(poke(_, _))
  step(1)
  (c.io.sum, sw).zipped.map(expect(_, _))
}

class PEArrayTest(c: PEArray) extends PeekPokeTester(c) {
  val r = scala.util.Random
  val f = List.fill(c.filterRowNum, c.filterLen)(r.nextInt(10) - 5)
  val i = List.fill(c.imgRowNum, c.imgLen)(r.nextInt(10) - 5)
  val swResult = SW.conv2d(f, i)

  (c.io.filterIn, f).zipped.map((iorow, frow) => {
    (iorow, frow).zipped.map(poke(_, _))
  })
  (c.io.imgIn, i).zipped.map((iorow, irow) => {
    (iorow, irow).zipped.map(poke(_, _))
  })
  step(1)
  print(swResult)
  print(c.io.sum.map(peek(_)))
  (c.io.sum, swResult).zipped.map((sumrow, swrow) => {
    (sumrow, swrow).zipped.map(expect(_, _))
  })
}


class PETester extends ChiselFlatSpec {
  val fLen = 5
  val iLen = 32
  val w = 16

  private val backendNames = if (false && firrtl.FileUtils.isCommandAvailable(Seq("verilator", "--version"))) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }

  "using --backend-name verilator" should "be an alternative way to run using verilator" in {
    if (backendNames.contains("verilator")) {
      Driver.execute(Array("--backend-name", "verilator"), () => new PE(fLen, iLen, w)) {
        c => new PETest(c)
      } should be(true)
    }
  }

  "Basic test using Driver.execute" should "be used as an alternative way to run specification" in {
    Driver(() => new PE(fLen, iLen, w)) {
      c => new PETest(c)
    } should be(true)
  }
}

class PEArrayTester extends ChiselFlatSpec {
  val finRow = 5
  val fLen = 5
  val iinRow = 32
  val iLen = 32
  val w = 16

  private val backendNames = if (false && firrtl.FileUtils.isCommandAvailable(Seq("verilator", "--version"))) {
    Array("firrtl", "verilator")
  }
  else {
    Array("firrtl")
  }

  "PEArray test use Basic " should "i dont know why to show this information" in {
    Driver(() => new PEArray(finRow, fLen, iinRow, iLen, w)) {
      c => new PEArrayTest(c)
    } should be(true)
    import fun._
    guoying()
  }
}
