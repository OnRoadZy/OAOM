#lang racket

(require "model/roll.rkt"
         "model/track.rkt"
         "model/ball.rkt"
         "model/value.rkt"
         "view/frame-main.rkt")

;初始化模型基础参数：
(set-oaom-init #:ro 1.3 #:ri 0.1 #:a 20 #:n 7
                 #:roll-m 0
                 #:track-m 0.01
                 #:ball-m 1)

;初始化视图参数：
(init-view)

;显示窗口：
(send main-frame show #t)
