package simulator

import breeze.linalg._
import scala.collection.mutable.ArrayBuffer

object State extends scala.Enumeration{
  type state = Value
  val clock = Value(1, "run")
  val noClock = Value(0, "not work")
}

class PE {
  import State._

  var filterLen: Int = 0
  var filterNum: Int = 0
  var filterIn: List[Int] = List.fill(0)(1)

  var imgLen: Int = 0
  var imgNum: Int = 0
  var imgIn: List[Int] = List.fill(0)(1)

  var nchannel: Int = 0

  var oSum = DenseMatrix.fill[Int](1,1)(0)

  var state = State.noClock

  def setAttribut(fLen:Int, fNum: Int, fIn: List[Int],
                  iLen: Int, iNum: Int, iIn:List[Int],
                  nchannel: Int, state: state): Unit ={
    filterNum = fNum
    filterLen = fLen
    filterIn = fIn
    imgNum = iNum
    imgLen = iLen
    imgIn = iIn
    this.nchannel = nchannel
    this.state = state
    oSum = DenseMatrix.fill[Int](filterNum, imgIn.length - filterIn.length/filterNum + 1)(0)
  }

  def shiftReg(x: List[Int]): List[Int] = {
    x.tail :+ x.head
  }

  def cal: DenseMatrix[Int] ={
    if(state == State.noClock){
      return DenseMatrix.fill(1,1)(0)
    }
    var filterReg = filterIn
    var fmapReg = imgIn.slice(0, filterReg.length)
    var pSumReg = DenseVector.fill[Int](100)(0)
    var oSumReg = DenseMatrix.fill[Int](filterNum, imgIn.length - filterIn.length/filterNum + 1)(0)
    var model = if(nchannel != 1 & filterNum == 1 & imgNum == 1){ 2 }         //多通道计算
                else if(nchannel == 1 & filterNum != 1 & imgNum == 1){ 1 }    //多filter 单img计算
                else if(nchannel ==1 & filterNum == 1 & imgNum == 1){ 0 }     //单filter 单img计算
                else {-1}
    this.oSum = model match {
      case 0 => {
        // 1 filter  1 image  and 1 channel, reuse filter
        assert(filterNum == 1 & nchannel == 1 & imgNum == 1)

        for (i <- 0 until imgIn.length - filterIn.length + 1) {
          for (j <- filterReg.indices) {
            pSumReg(i) += filterReg.head * fmapReg.head
            filterReg = shiftReg(filterReg)
            fmapReg = shiftReg(fmapReg)
          }
          oSumReg(0, i) = pSumReg(i)
          fmapReg = imgIn.slice(i + 1, i + 1 + filterReg.length)
        }
        oSumReg
      }

      case 1 => {
        // several filters and one img, reuse img
        assert(imgNum == 1 & nchannel == 1 & filterNum != 1)

        for (i <- 0 until imgIn.length - filterIn.length / filterNum + 1) {
          for (j <- Range(0, filterReg.length / filterNum)) {
            for (k <- 0 until filterNum) {
              pSumReg(i * filterNum + k) += filterReg.head * fmapReg.head
              filterReg = shiftReg(filterReg)
            }
            fmapReg = shiftReg(fmapReg)
          }
          oSumReg(0 until filterNum, i) := pSumReg(i * filterNum until (i + 1) * filterNum)
          fmapReg = imgIn.slice(i + 1, i + 1 + filterReg.length)
        }
        oSumReg
      }
      case 2 => {
        // servel channel
        assert(nchannel != 1)
        var oSumReg = DenseMatrix.fill[Int](nchannel, imgIn.length/nchannel - filterIn.length/nchannel + 1)(0)
        for (i <- 0 until imgIn.length/nchannel - filterIn.length / nchannel + 1) {
          for (j <- Range(0, filterReg.length/nchannel)) {
            for (k <- 0 until nchannel) {
              pSumReg(i * nchannel + k) += filterReg.head * fmapReg.head
              filterReg = shiftReg(filterReg)
              fmapReg = shiftReg(fmapReg)
            }
          }
          oSumReg(0 until nchannel, i) := pSumReg(i * nchannel until (i + 1) * nchannel)
          fmapReg = imgIn.slice(i + nchannel, i + nchannel + filterReg.length)
        }
        oSumReg
      }
      case _ => {
        println("Not compatible with currect compute mode")
        assert(false)
        DenseMatrix.fill[Int](1,1)(0)
      }
    }
    this.oSum
  }
}

object PEArrayGenerator{
  var fRow = 0
  var fLen = 0
  var iRow = 0
  var iLen = 0

  def getIput(filter: DenseMatrix[Int], img: DenseMatrix[Int]): Unit ={
    (fRow, fLen) = (filter.rows, filter.cols)
    (iRow, iLen) = (img.rows, img.cols)
  }
  def generateArray(): List[List[PE]] ={
    val PEArray = List.fill[PE](fRow, iRow - fRow + 1)(new PE())
    PEArray
  }
}


object SW {

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

  def convMode0(filter:List[Int], img: List[Int], sum: List[Int]):List[Int] = conv1d(filter, img, sum)

  def convMode1(filters:List[Int], filterNum: Int, img: List[Int], sum: List[Int]):List[List[Int]] = {
    val filterLen = filters.length
    val filter = List[List[Int]]().toBuffer
    val temp = filters.grouped(filterNum).toList
    for(i <- Range(0,filterNum)){
      filter.append(temp.map(_(i)))
    }
    filter.map(conv1d(_, img, sum)).toList
  }

  def convMode2(filters:List[Int], imgs: List[Int], nchannel: Int, sum: List[Int]):List[List[Int]] = {
    val filterLen = filters.length
    val filter = List[List[Int]]().toBuffer
    val img = List[List[Int]]().toBuffer
    val ftemp = filters.grouped(nchannel).toList
    val itemp = imgs.grouped(nchannel).toList
    for(i <- Range(0,nchannel)){
      filter.append(ftemp.map(_(i)))
      img.append(itemp.map(_(i)))
    }
    (filter, img).zipped.map(conv1d(_, _, sum)).toList
  }
}

object Main extends App{

  implicit def tuple2List(x: Product): List[Int] ={
    x.productIterator.toList.asInstanceOf[List[Int]]
  }

  val pe = new PE()
  println("---test mode0---")
  pe.setAttribut(
    3, 1, List(1,2,3),
    5, 1, List(1,2,3,4,5),
    1, State.clock
  )
  println(pe.cal)
  println(SW.convMode0((1,2,3), (1,2,3,4,5), (0,0,0,0)))
  println(pe.cal.toArray.toList == SW.convMode0((1,2,3), (1,2,3,4,5), (0,0,0,0)))

  println("---test mode1---")
  pe.setAttribut(
    4, 2, List(1,2,3,4),
    5, 1, List(1,2,3,4,5),
    1, State.clock
  )
  println(pe.cal)
  println(SW.convMode1((1,2,3,4), 2, (1,2,3,4,5), (0,0,0,0)))
  println(pe.cal.t.toArray.toList == SW.convMode1((1,2,3,4), 2, (1,2,3,4,5), (0,0,0,0)).flatten)

  println("---test mode2---")
  pe.setAttribut(
    4, 1, List(1,2,3,4),
    5, 1, List(1,2,3,4,5,6),
    2, State.clock
  )
  println(pe.cal)
  println(SW.convMode2((1,2,3,4),  (1,2,3,4,5,6), 2, (0,0,0,0)))
  println(pe.cal.t.toArray.toList == SW.convMode2((1,2,3,4),  (1,2,3,4,5,6), 2, (0,0,0,0)).flatten)

}
