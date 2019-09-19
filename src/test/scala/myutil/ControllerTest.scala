package myutil

import chisel3._
import chisel3.util._
import node.dataPackage
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class Controller2RAM() extends Module {
  val io = IO(new Bundle {
    val dout = Decoupled(new dataPackage())
    val go = Input(Bool())
    val bias = Output(SInt(8.W))
  })
  val ctrl = Module(new Controller())
  val ram = Module(new RAM(20, 280))

  ram.io <> ctrl.io.ram
  io.dout <> ctrl.io.dout
  //  io.dout.bits := ctrl.io.dout.bits
  //  io.dout.valid := ctrl.io.dout.valid
  //  ctrl.io.dout.ready := io.dout.ready
//  ctrl.io.bias := 0.S
  ctrl.io.readGo := io.go
  ctrl.io.dataDone := 0.U
  io.bias := ctrl.io.bias
}

class ControllerTest(c: Controller2RAM) extends PeekPokeTester(c) {
  poke(c.io.go, 1)
  poke(c.io.dout.ready, 1)
  step(3000)
}

class ControllerTester extends ChiselFlatSpec {
  "running with --generate-vcd-output on" should "create a vcd file from your test" in {
    iotesters.Driver.execute(
      Array("--generate-vcd-output", "on", "--target-dir", "test_run_dir/make_Controller_vcd",
        "--top-name", "make_Controller_vcd", "--backend-name", "verilator",
        // "-tmvf", "-full64 -cpp g++-4.8 -cc gcc-4.8 -LDFLAGS -Wl,-no-as-needed +memcbk  +vcs+dumparrays -debug_all"
      ),
      () => new Controller2RAM()
    ) {
      c => new ControllerTest(c)
    } should be(true)
  }
}
