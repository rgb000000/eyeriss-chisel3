package simulator

import breeze.linalg._
import scala.collection.mutable.ArrayBuffer

object State extends scala.Enumeration {
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

  var nchannel: Int = 1

  var oSum = DenseMatrix.fill[Int](1, 1)(0)

  var state = State.noClock

  def setAttribut(fLen: Int, fNum: Int, fIn: List[Int],
                  iLen: Int, iNum: Int, iIn: List[Int],
                  nchannel: Int, state: state): Unit = {
    filterNum = fNum
    filterLen = fLen
    filterIn = fIn
    imgNum = iNum
    imgLen = iLen
    imgIn = iIn
    this.nchannel = nchannel
    this.state = state
    oSum = DenseMatrix.fill[Int](filterNum, imgIn.length - filterIn.length / filterNum + 1)(0)
  }

  def setFilter(fIn: DenseVector[Int], fNum: Int): Unit ={
    this.filterLen = fIn.length
    this.filterNum = fNum
    this.filterIn = fIn.toArray.toList
    this.state = clock
  }

  def setImg(iIn: DenseVector[Int], iNum: Int): Unit ={
    this.imgLen = iIn.length
    this.imgNum = iNum
    this.imgIn = iIn.toArray.toList
    this.state = clock
  }

  def set(fIn: DenseVector[Int], fNum: Int,
          iIn: DenseVector[Int], iNum: Int,
          nchannel: Int): Unit ={
    setFilter(fIn, fNum)
    setImg(iIn, iNum)
    this.nchannel = nchannel
    this.state = clock
  }

  def shiftReg(x: List[Int]): List[Int] = {
    x.tail :+ x.head
  }

  def cal: DenseMatrix[Int] = {
    if (state == State.noClock) {
      return DenseMatrix.fill(1, 1)(0)
    }
    var filterReg = filterIn
    var fmapReg = imgIn.slice(0, filterReg.length)
    var pSumReg = DenseVector.fill[Int](10000)(0)
    var oSumReg = DenseMatrix.fill[Int](filterNum, imgIn.length - filterIn.length / filterNum + 1)(0)
    var model = if (nchannel != 1 & filterNum == 1 & imgNum == 1) {
      2
    } //多通道计算
    else if (nchannel == 1 & filterNum != 1 & imgNum == 1) {
      1
    } //多filter 单img计算
    else if (nchannel == 1 & filterNum == 1 & imgNum == 1) {
      0
    } //单filter 单img计算
    else {
      -1
    }
    println(s"mode: ${model}")
    println(s"filterNum ${filterNum}")
    println(s"imgNum ${imgNum}")
    println(s"nchannel ${nchannel}")
    println(s"fLen ${filterLen}")
    println(s"iLen ${imgLen}")
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
        var oSumReg = DenseMatrix.fill[Int](nchannel, imgIn.length / nchannel - filterIn.length / nchannel + 1)(0)
        for (i <- 0 until imgIn.length / nchannel - filterIn.length / nchannel + 1) {
          for (j <- Range(0, filterReg.length / nchannel)) {
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
        DenseMatrix.fill[Int](1, 1)(0)
      }
    }
    this.oSum
  }
}

class eyerissSystem(val PEArray: List[List[PE]]){
  def cal : DenseVector[DenseMatrix[Int]] = {
    PEArray.foreach(_.foreach(_.cal))
    val result = PEArray.map(_.map(_.oSum)).map(_.toArray).map(DenseVector(_)).reduce(_ + _)
    result
  }
}


object PEArrayGenerator {

  def getIput(filter: DenseMatrix[Int], img: DenseMatrix[Int]): Tuple4[Int, Int, Int, Int] = {
    (filter.rows,
      filter.cols,
      img.rows,
      img.cols)
  }

  def generateArray(filter: DenseMatrix[Int], img: DenseMatrix[Int],
                    filterNum: Int = 1, nchannel: Int = 1): List[List[PE]] = {
    val (fRow, fLen, iRow, iLen) = getIput(filter, img)
    val PEArray = List.fill[PE](fRow, iRow - fRow + 1)(new PE())

    // Set nchannel
    PEArray.foreach(_.foreach(_.nchannel = nchannel))

    // Set filter Row to One Row filter
    val filterList = ArrayBuffer[DenseVector[Int]]()
    filter.t(::,*).foreach(filterList.append(_))
    (PEArray, filterList).zipped.foreach((PERow, fRow)=>{
      PERow.foreach(_.setFilter(fRow, filterNum))
    })

    // Set filter Row to One Row image
    val imgList = ArrayBuffer[DenseVector[Int]]()
    img.t(::,*).foreach(imgList.append(_))
    var i = 0   //row
    var j = 0   //col
    PEArray.foreach((PERow) => {
      PERow.foreach((PE)=>{
        PE.setImg(imgList(i + j), 1)
        j += 1
      })
      i += 1
      j = 0
    })

    PEArray
  }
}


object SW {

  implicit def denseVectorInt2ListInt(dv: DenseVector[Int]):List[Int] = {
    dv.toArray.toList
  }

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

  def conv1d(filter: DenseVector[Int], img: DenseVector[Int], sumIn: DenseVector[Int]): DenseVector[Int] = {
    DenseVector(conv1d(filter.toArray.toList, img, sumIn).toArray)
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

  def conv2d(filter: DenseMatrix[Int], img: DenseMatrix[Int]): DenseMatrix[Int] = {
    // only suppost  square 正方形 conv
    assert(filter.rows == filter.cols)
    assert(img.rows == img.cols)

    val fSize = filter.rows
    val iSize = img.rows

    val result = DenseMatrix.fill[Int](iSize - fSize + 1, iSize - fSize + 1)(0)

    for(i <- Range(0, iSize - fSize + 1)){
      for(j <- Range(0, iSize - fSize + 1)){
        result(i, j) = sum(img(i to i + fSize - 1, j to j + fSize - 1) *:* filter)
      }
    }
    result
  }

  def convMode0(filter: List[Int], img: List[Int], sum: List[Int] = List.fill[Int](20)(0)): List[Int] = conv1d(filter, img, sum)
  def convMode0(filter: DenseVector[Int], img: DenseVector[Int], sum: DenseVector[Int]): DenseVector[Int] ={
    conv1d(filter, img, sum)
  }

  def convMode1(filters: List[Int], filterNum: Int, img: List[Int], sum: List[Int]): List[List[Int]] = {
    val filterLen = filters.length
    val filter = List[List[Int]]().toBuffer
    val temp = filters.grouped(filterNum).toList
    for (i <- Range(0, filterNum)) {
      filter.append(temp.map(_ (i)))
    }
    filter.map(conv1d(_, img, sum)).toList
  }
  def convMode1(filters: DenseVector[Int], filterNum: Int, img: DenseVector[Int],
                sum: DenseVector[Int]): DenseVector[DenseVector[Int]] = {
    DenseVector(convMode1(filters.toArray.toList, filterNum, img, sum).map((l:List[Int])=>{
      DenseVector(l.toArray)
    }).toArray)
  }

  def convMode2(filters: List[Int], imgs: List[Int], nchannel: Int, sum: List[Int]): List[Int] = {
    val filterLen = filters.length
    val filter = List[List[Int]]().toBuffer
    val img = List[List[Int]]().toBuffer
    val ftemp = filters.grouped(nchannel).toList
    val itemp = imgs.grouped(nchannel).toList
    for (i <- Range(0, nchannel)) {
      filter.append(ftemp.map(_ (i)))
      img.append(itemp.map(_ (i)))
    }
    def listAdd(a: List[Int],b: List[Int]): List[Int] ={
      (a,b).zipped.map(_+_)
    }
    (filter, img).zipped.map(conv1d(_, _, sum)).toList.reduce(listAdd(_,_))
  }

  def convMode2(filters: DenseVector[Int], imgs: DenseVector[Int], nchannel: Int,
                sum: DenseVector[Int]): DenseVector[DenseVector[Int]] = {
    DenseVector(convMode2(filters.toArray.toList, imgs, nchannel, sum).map((l: Int) => {
      DenseVector(l)
    }).toArray)
  }
}

object Main extends App {

  implicit def tuple2List(x: Product): List[Int] = {
    x.productIterator.toList.asInstanceOf[List[Int]]
  }

  val pe = new PE()
  println("---test mode0---")
  pe.setAttribut(
    3, 1, List(1, 2, 3),
    5, 1, List(1, 2, 3, 4, 5),
    1, State.clock
  )
  println("PE.cal -> ", pe.cal)
  println("SW.convMode0 list -> ", SW.convMode0((1, 2, 3), (1, 2, 3, 4, 5), (0, 0, 0, 0)))
  println("SW.convMode0 DV -> ", SW.convMode0(DenseVector(1, 2, 3), DenseVector(1, 2, 3, 4, 5), DenseVector(0, 0, 0, 0)))
  println(pe.cal.toArray.toList == SW.convMode0((1, 2, 3), (1, 2, 3, 4, 5), (0, 0, 0, 0)))

  println("---test mode1---")
  pe.setAttribut(
    4, 2, List(1, 2, 3, 4),
    5, 1, List(1, 2, 3, 4, 5),
    1, State.clock
  )
  println("PE.cal -> ", pe.cal)
  println("SW.convMode1 list -> ", SW.convMode1((1, 2, 3, 4), 2, (1, 2, 3, 4, 5), (0, 0, 0, 0)))
  println("SW.convMode1 DV -> ", SW.convMode1(DenseVector(1, 2, 3, 4), 2, DenseVector(1, 2, 3, 4, 5), DenseVector(0, 0, 0, 0)))
  println(pe.cal.t.toArray.toList == SW.convMode1((1, 2, 3, 4), 2, (1, 2, 3, 4, 5), (0, 0, 0, 0)).flatten)

  println("---test mode2---")
  pe.setAttribut(
    4, 1, List(1, 2, 3, 4),
    5, 1, List(1, 2, 3, 4, 5, 6),
    2, State.clock
  )
  println(pe.cal)
  println("SW.convMode2 list -> ", SW.convMode2((1, 2, 3, 4), (1, 2, 3, 4, 5, 6), 2, (0, 0, 0, 0)))
  println("SW.convMode0 DV -> ", SW.convMode2(DenseVector(1, 2, 3, 4), DenseVector(1, 2, 3, 4, 5, 6), 2, DenseVector(0, 0, 0, 0)))
  println(pe.cal.t.toArray.toList == SW.convMode2((1, 2, 3, 4), (1, 2, 3, 4, 5, 6), 2, (0, 0, 0, 0)))

  println("---test conv2d mode0---")
  var filter = DenseMatrix((1,2,3),(4,5,6),(7,8,9))
  var img = DenseMatrix((1,2,3,4,5),(2,3,4,5,6),(3,4,5,6,7),(4,5,6,7,8),(5,6,7,8,9))
  var PEArray = new eyerissSystem(PEArrayGenerator.generateArray(filter, img))

//  PEArray.PEArray.foreach((PERow)=>{
//    PERow.foreach((PE)=>{
//      println(PE.filterIn)
//      println(PE.imgIn)
//    })
//  })

  println("PEArray mode1 -> \n", PEArray.cal)
  println("SW.conv2d list -> ", SW.conv2d(List(List(1,2,3), List(4,5,6), List(7,8,9)), List(List(1,2,3,4,5),List(2,3,4,5,6),List(3,4,5,6,7),List(4,5,6,7,8),List(5,6,7,8,9))))
  println("SW.conv2d DM -> \n", SW.conv2d(filter, img))

  println("---test conv2d mode1---")
  filter = DenseMatrix((1,2,3,4),(4,5,6,7))
  img = DenseMatrix((1,2,3,4,5),(2,3,4,5,6),(3,4,5,6,7),(4,5,6,7,8),(5,6,7,8,9))
  PEArray = new eyerissSystem(PEArrayGenerator.generateArray(filter, img, 2 ))

//  PEArray.PEArray.foreach((PERow)=>{
//    PERow.foreach((PE)=>{
//      println(PE.filterIn)
//      println(PE.imgIn)
//      println(PE.filterNum)
//      println(PE.nchannel)
//    })
//  })

  println("PEArray mode1 -> \n", PEArray.cal)
  println("SW.conv2d list -> ", SW.conv2d(List(List(1,3), List(4,6)), List(List(1,2,3,4,5),List(2,3,4,5,6),List(3,4,5,6,7),List(4,5,6,7,8),List(5,6,7,8,9))))
  println("SW.conv2d DM -> \n", SW.conv2d(filter(::, 1 to filter.cols by 2), img))

  println("---test conv2d mode2---")
  filter = DenseMatrix((1,2,3,4),(4,5,6,7))
  img = DenseMatrix((1,2,3,4,5,6),(2,3,4,5,6,7),(3,4,5,6,7,8))
  PEArray = new eyerissSystem(PEArrayGenerator.generateArray(filter, img, 1, 2 ))

//    PEArray.PEArray.foreach((PERow)=>{
//      PERow.foreach((PE)=>{
//        println(PE.filterIn)
//        println(PE.imgIn)
//        println(PE.filterNum)
//        println(PE.nchannel)
//      })
//    })

  println("PEArray mode2 -> \n", PEArray.cal)
  println("SW.conv2d list -> ", SW.conv2d(List(List(1,3), List(4,6)), List(List(1,3,5),List(2,4,6),List(3,5,7))))
  println("SW.conv2d DM -> \n", SW.conv2d(filter(::, 1 to filter.cols by 2), img(::, 1 to img.cols by 2)))
}

object tempTest extends App{
//  println(SW.convMode1(List(1,2,3,4,5,6), 2, List(1,2,3,4,5), List(0,0,0,0,0,0,0,0,0)))
  println(SW.convMode2(List(1, 2, 3, 4, 5, 6), List(1, 2, 3, 4, 5, 6,7,8,9,10), 2, List(0, 0, 0, 0, 0, 0, 0, 0)))
}

