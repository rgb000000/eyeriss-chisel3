Eyeriss by Chisel3(under development)
=======================

> #### Chisel Tutorial [Tutorial](https://github.com/ucb-bar/chisel-tutorial)
> #### IDEA Guide [Guide](https://github.com/ucb-bar/chisel-template/wiki/IntelliJ-Installation-Guide)
> #### breeze Tutorial [Tutorial](https://github.com/scalanlp/breeze/wiki/Quickstart)


# Usage
1. Clone and initial
```sh
git clone https://github.com/s1104439112/eyeriss-chisel3
cd eyeriss-chisel3
git submodule update --init
```
2. Test project
### Did it work?
run this, to check
```sh
sbt 'testOnly gcd.GCDTester -- -z Basic'
```


>This tells the test harness to only run the test in GCDTester that contains the word Basic
There are a number of other examples of ways to run tests in there, but we just want to see that
one works.

You should see a whole bunch of output that ends with something like the following lines
```
[info] [0.001] SEED 1540570744913
test GCD Success: 168 tests passed in 1107 cycles in 0.067751 seconds 16339.24 Hz
[info] [0.050] RAN 1102 CYCLES PASSED
[info] GCDTester:
[info] GCD
[info] Basic test using Driver.execute
[info] - should be used as an alternative way to run specification
[info] using --backend-name verilator
[info] running with --is-verbose
[info] running with --generate-vcd-output on
[info] running with --generate-vcd-output off
[info] ScalaTest
[info] Run completed in 3 seconds, 184 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[info] Passed: Total 1, Failed 0, Errors 0, Passed 1
[success] Total time: 5 s, completed Oct 26, 2018 9:19:07 AM
```
If you see the above then...

### It worked!
You are ready to go. We have a few recommended practices and things to do.
* Use packages and following conventions for [structure](http://www.scala-sbt.org/0.13/docs/Directories.html) and [naming](http://docs.scala-lang.org/style/naming-conventions.html)
* Package names should be clearly reflected in the testing hierarchy
* Build tests for all your work.
* This template includes a dependency on the Chisel3 IOTesters, this is a reasonable starting point for most tests
* You can remove this dependency in the build.sbt file if necessary
* Change the name of your project in the build.sbt file
* Change your README.md

There are [instructions for generating Verilog](https://github.com/freechipsproject/chisel3/wiki/Frequently-Asked-Questions#get-me-verilog) on the Chisel wiki.

Some backends (verilator for example) produce VCD files by default, while other backends (firrtl and treadle) do not.
You can control the generation of VCD files with the `--generate-vcd-output` flag.

To run the simulation and generate a VCD output file regardless of the backend:
```bash
sbt 'test:runMain gcd.GCDMain --generate-vcd-output on'
```

To run the simulation and suppress the generation of a VCD output file:
```bash
sbt 'test:runMain gcd.GCDMain --generate-vcd-output off'
```
