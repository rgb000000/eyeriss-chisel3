package myutil

import chisel3._
import chisel3.util._
import chisel3.experimental._

object addTree {

  // FIXME: attemplt to use T <: Data, but there is a error which indicate Data doens't have "+" function, Bits is the same
  def apply(seq: List[UInt]): UInt = {
    def groupAndAdd(seq: List[UInt]): List[UInt] = {
      seq.grouped(2).toList.map(_ match {
        case a: List[UInt] if a.length == 1 => {
          a(0)
        }
        case a: List[UInt] if a.length > 1 => {
          a(0) +& a(1)
        }
      })
    }

    var res = groupAndAdd(seq)
    while (res.length != 1) {
      res = groupAndAdd(res)
    }
    res.head
  }

  def apply(seq: List[SInt]): SInt = {
    def groupAndAdd(seq: List[SInt]): List[SInt] = {
      seq.grouped(2).toList.map(_ match {
        case a: List[SInt] if a.length == 1 => {
          a(0)
        }
        case a: List[SInt] if a.length > 1 => {
          a(0) +& a(1)
        }
      })
    }

    var res = groupAndAdd(seq)
    while (res.length != 1) {
      res = groupAndAdd(res)
    }
    res.head
  }
}

// SInt add tree module with DecoupleIO
class addTreeDecoupleIO(inNum: Int, w: Int = 8) extends Module {
  val io = IO(new Bundle {
    val seq = Vec(inNum, Flipped(DecoupledIO(SInt(w.W))))
    val out = DecoupledIO(SInt(w.W))
  })

  val qIn = Wire(DecoupledIO(io.seq.head.bits.cloneType))
  val q = Queue(qIn)
  io.out <> q

  // valid
  qIn.valid := io.seq.map(_.valid).reduce(_ & _)

  //bits
  qIn.bits := addTree(io.seq.map(_.bits).toList)

  // ready
  io.seq.foreach(_.ready := qIn.ready & io.seq.map(_.valid).reduce(_ & _))

}
