//// row data recover test
//package ram
//
//import chisel3._
//import chisel3.util._
//import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
//import config._
//
//class RowDataRcvTests(c: RowDataRecover) extends PeekPokeTester(c){
//  val length = 16
//  val in = List(1,2,3)
//  poke(c.io.length, length)
//  (c.io.rdata, in).zipped.foreach(poke(_, _))
//  poke(c.io.go, 1)
//  step(64)
//
//}
//
//class RowDataRcvTester extends ChiselFlatSpec {
//  implicit val p = new SmallWidthConfig
//  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
//    iotesters.Driver.execute(
//      Array(
//        "--generate-vcd-output", "on",
//        "--target-dir", "test_run_dir/make_rdr_vcd",
//        "--top-name", "make_rdr_vcd",
//        "--backend-name", "verilator"
//      ),
//      () => new RowDataRecover()
//    ) {
//      c => new RowDataRcvTests(c)
//    } should be(true)
//  }
//}
