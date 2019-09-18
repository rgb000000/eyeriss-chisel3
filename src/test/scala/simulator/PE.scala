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

  def setFilter(fIn: DenseVector[Int], fNum: Int): Unit = {
    this.filterLen = fIn.length
    this.filterNum = fNum
    this.filterIn = fIn.toArray.toList
    this.state = clock
  }

  def setImg(iIn: DenseVector[Int], iNum: Int): Unit = {
    this.imgLen = iIn.length
    this.imgNum = iNum
    this.imgIn = iIn.toArray.toList
    this.state = clock
  }

  def set(fIn: DenseVector[Int], fNum: Int,
          iIn: DenseVector[Int], iNum: Int,
          nchannel: Int): Unit = {
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

class eyerissSystem(val PEArray: List[List[PE]]) {
  def cal: DenseVector[DenseMatrix[Int]] = {
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
    filter.t(::, *).foreach(filterList.append(_))
    (PEArray, filterList).zipped.foreach((PERow, fRow) => {
      PERow.foreach(_.setFilter(fRow, filterNum))
    })

    // Set filter Row to One Row image
    val imgList = ArrayBuffer[DenseVector[Int]]()
    img.t(::, *).foreach(imgList.append(_))
    var i = 0 //row
    var j = 0 //col
    PEArray.foreach((PERow) => {
      PERow.foreach((PE) => {
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

  implicit def denseVectorInt2ListInt(dv: DenseVector[Int]): List[Int] = {
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

  def conv2d(filter: DenseMatrix[Int], img: DenseMatrix[Int], activate: Boolean = false): DenseMatrix[Int] = {
    // only suppost  square 正方形 conv
    assert(filter.rows == filter.cols)
    //    assert(img.rows == img.cols)

    val fRow = filter.rows
    val fCol = filter.cols
    val iRow = img.rows
    val iCol = img.cols

    val result = DenseMatrix.fill[Int](iRow - fRow + 1, iCol - fCol + 1)(0)

    for (i <- Range(0, iRow - fRow + 1)) {
      for (j <- Range(0, iCol - fCol + 1)) {
        result(i, j) = sum(img(i to i + fRow - 1, j to j + fCol - 1) *:* filter)
      }
    }
    if (activate) {
      ConvTools.relu(result)
    } else {
      result
    }
  }

  def convMode0(filter: List[Int], img: List[Int], sum: List[Int] = List.fill[Int](2000)(0)): List[Int] = conv1d(filter, img, sum)

  def convMode0(filter: DenseVector[Int], img: DenseVector[Int], sum: DenseVector[Int]): DenseVector[Int] = {
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
    DenseVector(convMode1(filters.toArray.toList, filterNum, img, sum).map((l: List[Int]) => {
      DenseVector(l.toArray)
    }).toArray)
  }

  def listAdd(a: List[Int], b: List[Int]): List[Int] = {
    (a, b).zipped.map(_ + _)
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
    (filter, img).zipped.map(conv1d(_, _, sum)).toList.reduce(listAdd(_, _))
  }

  def convMode2(filters: DenseVector[Int], imgs: DenseVector[Int], nchannel: Int,
                sum: DenseVector[Int]): DenseVector[DenseVector[Int]] = {
    DenseVector(convMode2(filters.toArray.toList, imgs, nchannel, sum).map((l: Int) => {
      DenseVector(l)
    }).toArray)
  }

  def convGeneral(filters: DenseVector[Int], filterNum: Int, imgs: DenseVector[Int], imgNum: Int, nchannel: Int,
                  sum: List[Int] = List.fill[Int](10)(0)): DenseMatrix[Int] = {
    //    assert(filters.length == nchannel * filterNum)
    println(s"filters: ${filters} __ ${filters.length}")
    println(s"filterNum: ${filterNum}")
    println(s"imgs: ${imgs} __ ${imgs.length}")
    println(s"imgNum: ${imgNum}")
    println(s"nchannel: ${nchannel}")
    var filter = DenseMatrix.fill[Int](filterNum, filters.length / (filterNum * nchannel))(0)
    var img = DenseMatrix.fill[Int](imgNum, imgs.length / (imgNum * nchannel))(0)
    val results = List[DenseMatrix[Int]]().toBuffer
    for (i <- Range(0, nchannel)) {
      val result = DenseMatrix.fill[Int](filterNum * imgNum, imgs.length / (imgNum * nchannel) -
        filters.length / (filterNum * nchannel) + 1)(0)
      for (j <- Range(0, filterNum)) {
        //        println(s"filter cols: ${filter.cols}")
        //        println(s"local filter cols: ${filters(j + i * filterNum to filters.length - 1 by nchannel * filterNum).length}")
        filter(j, ::) := filters(j + i * filterNum to filters.length - 1 by nchannel * filterNum).t
      }
      for (j <- Range(0, imgNum)) {
        //        img(j, ::) := imgs(j + imgs.length / (imgNum * nchannel) * i to j + imgs.length / (imgNum * nchannel) * (i + 1) by nchannel)
        img(j, ::) := imgs(j * (imgs.length / (imgNum)) + i to (j + 1) * (imgs.length / (imgNum)) - 1 + i by nchannel).t
      }
      //      println("filter")
      //      println(filter)
      //      println("img")
      //      println(img)
      //      filter.t(::,*).foreach(println(_))
      var fcnt = 0
      var icnt = 0
      filter(*, ::).foreach((f: DenseVector[Int]) => {
        img(*, ::).foreach((x: DenseVector[Int]) => {
          //                    println("result")
          //                    println(f)
          //                    println(x)
          //          println(result.cols)
          result(fcnt + icnt * filterNum, ::) := DenseVector(convMode0(f, x): _*).t
          icnt += 1
        })
        fcnt += 1
        icnt = 0
      })
      //      println("------")
      //      println(result)
      results.append(result)
    }
    //    println(results)
    results.reduce(_ + _)
  }

  def randomMatrix(shape: (Int, Int)): DenseMatrix[Int] = {
    val random = scala.util.Random
    val a = DenseMatrix.fill(shape._1, shape._2)(random.nextInt(20) - 10)
    a
  }

  def fd2List(data: DenseMatrix[DenseMatrix[Int]], filter_0_Img_1: Int): List[List[Int]] = {
    if (filter_0_Img_1 == 0) {
      // filter
      val channel = data.rows
      val num = data.cols
      val singleLen = data(0, 0).cols
      var rows = data(0, 0).rows
      val list = List[Int]().toBuffer
      val l = List[List[Int]]().toBuffer
      for (i <- Range(0, singleLen * singleLen)) {
        for (j <- Range(0, channel)) {
          for (k <- Range(0, num)) {
            list.append(data(j, k)(i / singleLen, i % singleLen))
          }

        }
      }
      val list2d = List[List[Int]]().toBuffer
      for (i <- Range(0, data(0, 0).rows)) {
        val rowLen = data.size * data(0, 0).cols
        list2d.append(list.slice(i * rowLen, (i + 1) * rowLen).toList)
      }
      list2d.toList
    } else {
      // img
      val channel = data.rows
      val num = data.cols
      val singleLen = data(0, 0).cols
      var rows = data(0, 0).rows
      val list = List[Int]().toBuffer
      for (l <- Range(0, rows)) {
        for (i <- Range(0, num)) {
          for (j <- Range(0, singleLen)) {
            for (k <- Range(0, channel)) {
              list.append(data(k, i)(l, j))
            }
          }
        }
      }
      val list2d = List[List[Int]]().toBuffer

      for (i <- Range(0, data(0, 0).rows)) {
        val rowLen = data.size * data(0, 0).cols
        list2d.append(list.slice(i * rowLen, (i + 1) * rowLen).toList)
      }
      list2d.toList
    }
  }

  // DM(channel, num)(height, width)
  def conv4d(filter: DenseMatrix[DenseMatrix[Int]], img: DenseMatrix[DenseMatrix[Int]],
             activate: Boolean = false, depthwise: Boolean = false, bias: DenseMatrix[Int] = DenseMatrix.fill(1, 1)(0)):
  DenseMatrix[DenseMatrix[Int]] = {
    assert(filter.rows == img.rows) // channelIn must ==
    val channelIn = filter.rows
    val channelOut = filter.cols
    val imgNum = img.cols
    val result = DenseMatrix.fill(channelOut, imgNum)(DenseMatrix.fill(img(0, 0).rows - filter(0, 0).rows + 1,
      img(0, 0).cols - filter(0, 0).cols + 1)(0))
    println(s"channelIn: ${channelIn}")
    println(s"channelOut: ${channelOut}")
    println(s"imgNum: ${imgNum}")
    for (i <- Range(0, channelOut)) {
      for (j <- Range(0, imgNum)) {
        //        println((filter(::, i).toArray, img(::, j).toArray).zipped.map(conv2d(_, _)).reduce(_ + _))
        result(i, j) := (filter(::, i).toArray, img(::, j).toArray).zipped.map(conv2d(_, _, depthwise)).reduce(_ + _)
      }
    }
    for (i <- Range(0, channelOut)) {
      for (j <- Range(0, imgNum)) {
        //        println((filter(::, i).toArray, img(::, j).toArray).zipped.map(conv2d(_, _)).reduce(_ + _))
        result(i, j) := result(i, j) + bias(0, i)
      }
    }
    if (activate) {
      result.map(ConvTools.relu(_))
    } else {
      result
    }
  }

}

object ConvTools {
  def relu(array: DenseMatrix[Int]): DenseMatrix[Int] = {
    array.map((x) => {
      if (x > 0) {
        x
      } else {
        0
      }
    })
  }

  def pooling(array: DenseMatrix[Int], size: Int = 2, activation: Int = 255): DenseMatrix[Int] = {
    // aim to squene matrix
    assert(array.cols % size == 0 & array.rows % size == 0 & array.rows == array.cols)
    val temp = DenseMatrix.fill(array.rows / size, array.cols / size)(DenseMatrix.fill(size, size)(0))
    for (i <- Range(0, (array.rows / size) * (array.rows / size))) {
      val x = i / (array.rows / size)
      val y = i % (array.rows / size)
      temp(x, y) = array(x * size to (x + 1) * size - 1, y * size to (y + 1) * size - 1)
    }
    temp.map(max(_)) / activation
  }

  def pooling2(array: DenseMatrix[Int], size: Int = 2, activation: Int = 255): DenseMatrix[Int] = {
    // aim to squene matrix
    assert(array.cols % size == 0 & array.rows % size == 0 & array.rows == array.cols)
    val temp = DenseMatrix.fill(array.rows / size, array.cols / size)(DenseMatrix.fill(size, size)(0))
    for (i <- Range(0, (array.rows / size) * (array.rows / size))) {
      val x = i / (array.rows / size)
      val y = i % (array.rows / size)
      temp(x, y) = array(x * size to (x + 1) * size - 1, y * size to (y + 1) * size - 1)
    }
    temp.map(max(_))
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
  var filter = DenseMatrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
  var img = DenseMatrix((1, 2, 3, 4, 5), (2, 3, 4, 5, 6), (3, 4, 5, 6, 7), (4, 5, 6, 7, 8), (5, 6, 7, 8, 9))
  var PEArray = new eyerissSystem(PEArrayGenerator.generateArray(filter, img))

  //  PEArray.PEArray.foreach((PERow)=>{
  //    PERow.foreach((PE)=>{
  //      println(PE.filterIn)
  //      println(PE.imgIn)
  //    })
  //  })

  println("PEArray mode1 -> \n", PEArray.cal)
  println("SW.conv2d list -> ", SW.conv2d(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)), List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5, 6), List(3, 4, 5, 6, 7), List(4, 5, 6, 7, 8), List(5, 6, 7, 8, 9))))
  println("SW.conv2d DM -> \n", SW.conv2d(filter, img))

  println("---test conv2d mode1---")
  filter = DenseMatrix((1, 2, 3, 4), (4, 5, 6, 7))
  img = DenseMatrix((1, 2, 3, 4, 5), (2, 3, 4, 5, 6), (3, 4, 5, 6, 7), (4, 5, 6, 7, 8), (5, 6, 7, 8, 9))
  PEArray = new eyerissSystem(PEArrayGenerator.generateArray(filter, img, 2))

  //  PEArray.PEArray.foreach((PERow)=>{
  //    PERow.foreach((PE)=>{
  //      println(PE.filterIn)
  //      println(PE.imgIn)
  //      println(PE.filterNum)
  //      println(PE.nchannel)
  //    })
  //  })

  println("PEArray mode1 -> \n", PEArray.cal)
  println("SW.conv2d list -> ", SW.conv2d(List(List(1, 3), List(4, 6)), List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5, 6), List(3, 4, 5, 6, 7), List(4, 5, 6, 7, 8), List(5, 6, 7, 8, 9))))
  println("SW.conv2d DM -> \n", SW.conv2d(filter(::, 1 to filter.cols by 2), img))

  println("---test conv2d mode2---")
  filter = DenseMatrix((1, 2, 3, 4), (4, 5, 6, 7))
  img = DenseMatrix((1, 2, 3, 4, 5, 6), (2, 3, 4, 5, 6, 7), (3, 4, 5, 6, 7, 8))
  PEArray = new eyerissSystem(PEArrayGenerator.generateArray(filter, img, 1, 2))

  //    PEArray.PEArray.foreach((PERow)=>{
  //      PERow.foreach((PE)=>{
  //        println(PE.filterIn)
  //        println(PE.imgIn)
  //        println(PE.filterNum)
  //        println(PE.nchannel)
  //      })
  //    })

  println("PEArray mode2 -> \n", PEArray.cal)
  println("SW.conv2d list -> ", SW.conv2d(List(List(1, 3), List(4, 6)), List(List(1, 3, 5), List(2, 4, 6), List(3, 5, 7))))
  println("SW.conv2d DM -> \n", SW.conv2d(filter(::, 1 to filter.cols by 2), img(::, 1 to img.cols by 2)))
}

object tempTest extends App {
  //  println(SW.convMode1(List(1,2,3,4,5,6), 2, List(1,2,3,4,5), List(0,0,0,0,0,0,0,0,0)))
  //    println(SW.convMode2(List(1, 2, 3, 4, 5, 6), List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2, List(0, 0, 0, 0, 0, 0, 0, 0)))
  implicit def list2dv(x: List[Int]): DenseVector[Int] = {
    DenseVector(x: _*)
  }

  println(SW.convGeneral(Range(1, 13).toList, 3, Range(1, 7).toList ::: Range(1, 7).toList ::: Range(1, 7).toList, 3, 2, List.fill[Int](20)(0)))
}


object tempTest2 extends App {
  val filter = List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))
  val img = List(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5))
  println(SW.conv2d(filter, img))
}

object tempTest3 extends App {
  //  val filter = DenseMatrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
  //  val img = DenseMatrix((1, 2, 3, 4, 5), (6, 7, 8, 9, 10), (1, 2, 3, 4, 5), (1, 2, 3, 4, 5), (1, 2, 3, 4, 5))
  //  SW.conv4d(DenseMatrix.fill(3, 3)(SW.randomMatrix(3, 3)), DenseMatrix.fill(3, 3)(SW.randomMatrix(5, 5)))
  //  println(SW.fd2List(DenseMatrix.fill(1, 1)(filter), 0))
  val filter = DenseMatrix.fill(2, 2)(DenseMatrix.fill(2, 2)(0))
  val img = DenseMatrix.fill(2, 2)(DenseMatrix.fill(3, 3)(0))
  filter(0, 0) = DenseMatrix((3, -2), (1, 2))
  filter(0, 1) = DenseMatrix((1, 2), (-3, -1))
  filter(1, 0) = DenseMatrix((0, 1), (2, -3))
  filter(1, 1) = DenseMatrix((1, -1), (3, 1))

  img(0, 0) = DenseMatrix((2, 1, 0), (1, 1, 2), (2, 0, -1))
  img(0, 1) = DenseMatrix((-1, 0, 1), (2, 1, 3), (2, -3, 0))
  img(1, 0) = DenseMatrix((1, -2, 2), (3, 0, 3), (2, 1, 3))
  img(1, 1) = DenseMatrix((2, 1, 3), (1, 1, 1), (2, -1, 3))

  println(SW.conv4d(filter, img))
  println(SW.fd2List(filter, 0))
  println(SW.fd2List(img, 1))
}

object convNotSquene extends App {
  val filter = DenseMatrix.fill(1, 6)(DenseMatrix.fill(5, 5)(0))
  for (i <- Range(0, 6)) {
    filter(0, i) = Data.flts1(i)
  }
  val img = DenseMatrix.fill(1, 1)(DenseMatrix.fill(11, 32)(0))
  img(0, 0) = Data.pics(0 to 11, ::)

  println(SW.conv4d(filter, img))
}

object MNIST extends App {
  val pic = Data.pics
  //  println(pic)
  val flt1 = Data.flts1
  //  println(flt1(5))
  val flt2 = Data.flts2
  //  println(flt2(5))
  val fc1W = Data.fc1
  //  println(fc1)
  val fc2W = Data.fc2
  //  println(fc2)
  val fc3W = Data.fc3
  //  println(fc3)

  var img = DenseMatrix.fill(1, 1)(pic)
  var f1 = DenseMatrix.fill(1, 6)(DenseMatrix.fill(5, 5)(0))
  for (x <- Range(0, 6)) {
    f1(0, x) = flt1(x)
    //    println(f1(0,x))
  }
  var conv1out = SW.conv4d(f1, img, true)
  conv1out = conv1out.map(ConvTools.pooling(_))
  println(conv1out(0, 0))
  println(conv1out.rows, conv1out.cols)
  println(conv1out(0, 0).rows, conv1out(0, 0).cols)

  var f2 = DenseMatrix.fill(6, 16)(DenseMatrix.fill(5, 5)(0))
  for (x <- Range(0, 16)) {
    for (y <- Range(0, 6)) {
      f2(y, x) = flt2(x)
    }
  }
  var conv2out = SW.conv4d(f2, conv1out, true, true)
  println(conv2out(0, 0))
  conv2out = conv2out.map(ConvTools.pooling(_))
  println(conv2out(0, 0))

  var fc1 = DenseMatrix(conv2out.toArray.toList.map(_.t.flatten().toArray.toList).reduce(_ ::: _)) * fc1W
  fc1 = fc1.map(_ / 255)
  println("fc1 \n", fc1)
  var fc2 = fc1 * fc2W
  println("fc2 \n", fc2)
  var fc3 = fc2 * fc3W
  println("fc3 \n", fc3)

  println(argmax(fc3))
}

object dataSplit extends App {
  val a: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(3)(ArrayBuffer.fill(256)(0))
  for (i <- Range(0, 3)) {
    for (j <- Range(0, 256)) {
      a(i)(j) = j
    }
  }
  val b = a.map(_.grouped(32).toList.toBuffer)
  var cnt = 0
  for (i <- Range(0, 256 / 32 - 1)) {
    b.foreach((x) => {
      println(x(cnt))
    })
    cnt += 1
  }

}

object mapTest extends App {
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

  val l = List(
    List(1, 2, 3),
    List(4, 5, 6),
    List(7, 8, 9)
  )
  val l2 = l.map((x) => {
    x.map(saturationSW(_))
  })
  println(l2)
  println(1.6.toInt)
}

object conv4dTest extends App {
  var filter = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
  var img = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
  var filterNum = 1
  var imgNum = 1
  var nchannel = 64
  var fLen = 3
  var iLen = 34 // padding = 1
  var maxLen = 0
  filter = DenseMatrix.fill(nchannel, filterNum)(SW.randomMatrix((fLen, fLen)))
  img = DenseMatrix.fill(nchannel, imgNum)(SW.randomMatrix((iLen, iLen)))
  maxLen = if (filterNum * fLen * nchannel > imgNum * iLen * nchannel) {
    filterNum * fLen * nchannel
  } else {
    imgNum * iLen * nchannel
  }
  println(maxLen.toString)
  //  require(maxLen < 255)
  println(filter(0, 0).toString)
  println(img(0, 0).toString)

  var sw = SW.conv4d(filter, img, true)
  sw.map((x) => {
    print(x)
    println()
  })
}