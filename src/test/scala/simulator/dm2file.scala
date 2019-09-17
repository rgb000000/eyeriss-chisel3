package simulator

import breeze.linalg._
import java.io._

object DM2file extends App {
  var filter = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
  var img = DenseMatrix.fill(3, 3)(DenseMatrix.fill(3, 3)(0))
  var filterNum = 1
  var imgNum = 1
  var nchannel = 64
  var fLen = 3
  var iLen = 34 // padding = 1
  var maxLen = 0
  var bias = 3
  filter = DenseMatrix.fill(nchannel, filterNum)(SW.randomMatrix((fLen, fLen)))
  img = DenseMatrix.fill(nchannel, imgNum)(SW.randomMatrix((iLen, iLen)))
  val filter2d = SW.fd2List(filter, 0)
  val img2d = SW.fd2List(img, 1)

  val w = new PrintWriter(new File("test.txt"))
  filter2d.foreach((l)=>{
    l.foreach(
      (num) => {
        w.write(f"${num.toByte}%02x".toUpperCase() + "\n")
      }
    )
  })

  val img2d_group = img2d.map(_.grouped(34).toList)
  for(i <- img2d_group(0).indices){
    for(j <- img2d_group.indices){
      var data35: String = ""
      for(k <- img2d_group(j)(i).indices){
        data35 = f"${img2d_group(j)(i)(k).toByte}%02x".toUpperCase() + data35
      }
      w.write(data35 + "\n")
    }
  }

  w.close()
}