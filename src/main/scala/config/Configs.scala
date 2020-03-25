package config

import axi._

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

case class ShellParams(
                        hostParams: AXIParams,
                        memParams: AXIParams)

case object ShellKey extends Field[ShellParams]

case class BRAMParams(
                       addrW:Int,
                       dataW:Int)

case object BRAMKey extends Field[BRAMParams]



class DefaultConfig extends Config((site, here, up) => {
  case FilterW => 8
  case ImgW => 8
  case BiasW => 8
  case OSumW => 8
  case AccW => 8

  case Shape => (3, 3)

  case FilterSpadDepth => 16  // just for 1 out channel
  case ImgSpadDepth => 16
  case PSumMemDepth => 16

  case RegFileW => 8
  case RegFileDepth => 8

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

  case BRAMKey => BRAMParams(
    addrW = 16,
    dataW = 64     // PEn N
  )

})
