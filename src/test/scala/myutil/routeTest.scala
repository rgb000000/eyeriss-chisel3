package myutil

import chisel3._
import chisel3.util._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

object getTopVerilog extends App{
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_route2_verilog"), () => new Route)
}

object getTopVerilog2 extends App{
  chisel3.Driver.execute(Array("--target-dir", "test_run_dir/make_bramctrl_verilog"), () => new BramController)
}
