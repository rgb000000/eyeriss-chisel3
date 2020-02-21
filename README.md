Eyeriss by Chisel3(under development)
=======================

> #### Chisel Tutorial [Tutorial](https://github.com/ucb-bar/chisel-tutorial)
> #### IDEA Guide [Guide](https://github.com/ucb-bar/chisel-template/wiki/IntelliJ-Installation-Guide)
> #### breeze Tutorial [Tutorial](https://github.com/scalanlp/breeze/wiki/Quickstart)


# Usage
## 1. Clone and initial
```sh
git clone https://github.com/s1104439112/eyeriss-chisel3
cd eyeriss-chisel3
git checkout MNIST
git submodule update --init
```
## 2. Test project
run this, to check your environmrnt
```sh
sbt 'testOnly node.MNISTTester'
```
now can find *test_run_dir/make_MNIST_vcd* dir, use gtkwave or verdi to open *PEArray.vcd* in this dir

You should see the following lines
```
[info] [51.767] List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
[info] [51.767] 
[info] [51.767] sw: 
[info] [51.767] 152  97   0    0   26  48   0    0    0    0  
53   106  27   0   0   117  172  153  2    0  
38   192  122  87  97  105  182  215  127  0  
[info] [51.767] 
[info] [51.856] jj reduce: 30
[info] [51.856] sw1d: 30
[info] [51.857] ===============ERROR: 0======================
[info] [51.894] (0,2)
[info] [51.894] total cnt: 14304
[info] [51.894] error cnt: 0
[info] [51.894] conv1 cnt: 4704
[info] [51.894] conv2 cnt: 9600
Enabling waves..
Exit Code: 0
[info] [59.453] RAN 94524 CYCLES PASSED
[info] MNISTTester:
[info] running with --generate-vcd-output on
[info] - should create a vcd file from your test
[info] ScalaTest
[info] Run completed in 1 minute, 50 seconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 1, failed 0, canceled 0, ignored 0, pending 0
[info] All tests passed.
[info] Passed: Total 1, Failed 0, Errors 0, Passed 1
[success] Total time: 113 s, completed 2019-6-17 15:18:59
```
If you see the above then...

### It worked!

# TODO
- [x] 硬件功能模块开发
- [x] 硬件功能模块测试
- [ ] 硬件上板测试
- [x] 运行MNIST仿真测试(手动调度,硬件只负责卷积计算)在MNIST分支中
- [ ] 添加对fc，pooling支持
- [ ] global buffer功能开发
- [ ] 软件调度算法


# Welcome to join Hardcore Player
A man walks fast, and a group goes far

欢迎入群交流

![qq group](https://github.com/s1104439112/eyeriss-chisel3/blob/master/img/chisel_QQ_group.png)
