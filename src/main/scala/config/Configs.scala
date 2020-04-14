package config

import axi._
import java.nio.file.Paths

case object FilterW extends Field[Int]

case object ImgW extends Field[Int]

case object BiasW extends Field[Int]

case object OSumW extends Field[Int]

case object AccW extends Field[Int]

case object Shape extends Field[(Int, Int)]

case object FilterSpadDepth extends Field[Int]

case object ImgSpadDepth extends Field[Int]

case object PSumMemDepth extends Field[Int]

case object RegFileW extends Field[Int]

case object RegFileDepth extends Field[Int]

case object WriterBRAMW extends Field[Int]

case class ShellParams(
                        hostParams: AXIParams,
                        memParams: AXIParams)

case object ShellKey extends Field[ShellParams]

case class BRAMParams(
                       addrW:Int,
                       dataW:Int)

case object BRAMKey extends Field[BRAMParams]

case object FilterMEMPath extends Field[String]

case object FeatureMEMPath extends Field[String]

case object MaxChannel extends Field[Int]

class DefaultConfig(maxChannel: Int = 8, col: Int = 3) extends Config((site, here, up) => {
  case FilterW => 8
  case ImgW => 8
  case BiasW => 8
  case OSumW => 8
  case AccW => 8
  case WriterBRAMW => 128
  case MaxChannel => maxChannel

  case Shape => (3, col)

  case FilterSpadDepth => here(MaxChannel) + 1  // just for 1 out channel
  case ImgSpadDepth => 4
  case PSumMemDepth => here(MaxChannel)

  case RegFileW => 8
  case RegFileDepth => 16

  case ShellKey => ShellParams(
    hostParams = AXIParams(coherent = false,
      addrBits = 16,
      dataBits = 32,
      lenBits = 8,
      userBits = 1),
    memParams = AXIParams(coherent = false,
      addrBits = 64,
      dataBits = 64,
      lenBits = 8,
      userBits = 1)
  )

  case FilterMEMPath =>
    val path = Paths.get("./schedule/python/filterMEM.hex").toAbsolutePath.toString
    println(path)
    path

  case FeatureMEMPath =>
    val path = Paths.get("./schedule/python/featureMEM.hex").toAbsolutePath.toString
    println(path)
    path

  case BRAMKey => BRAMParams(
    addrW = 16,
    dataW = here(MaxChannel) * here(FilterW)     // PEn N
  )
})

class SmallWidthConfig extends DefaultConfig(8, 3)

class BigWidthConfig extends DefaultConfig(64, 14)
