#lang racket

(require "model/roll.rkt"
         "model/track.rkt"
         "model/ball.rkt"
         "model/value.rkt"
         "view/frame-main.rkt")

;初始化视图参数：
(init-view)

;显示窗口：
(send main-frame show #t)
;显示初始化信息:
(send statuebar/message set-label
      "程序准备就绪，请设置各运行参数并点击“R运行”按钮开始运行。")