// Test Conv

package conv

import org.scalatest._
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg._

import scala.collection.mutable.ArrayBuffer
import simulator.SW

object GetVerilog extends App {
  println("hi")
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"), () => new PE(3, 5, 8))
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir"),
    () => new PEArray(2, 2, 3, 3, 16))
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
