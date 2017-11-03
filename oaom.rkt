#lang racket
(require "wheel.rkt")

;;初始化圆盘：
(define whl
  (new wheel%
       [R0 3];圆盘半径m。
       [m0 1];圆盘质量Kg。
       [nt0 7];轨道数。
       [dr0 10];第一个轨道的近圆心端点起始角度(度)。
       [d0-int 1];计算的转动角度间隔。
       [R0-track 6];轨道外圆半径（m）。
       [r0-track 1];轨道内圆半径（m）。
       [m0-track 1];轨道质量（Kg）。
       [m0-ball 3]));重力球质量（Kg）。

;;运行圆盘：
(send whl auto-run-step 300)
;(send whl auto-run-time 300 1)
