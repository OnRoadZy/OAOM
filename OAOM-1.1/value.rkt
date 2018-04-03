#lang racket

(provide oaom-init
         set-oaom-init
         (struct-out oaom-struct)
         g
         DT DC)

;ro：外圆半径(米)；ri：内圆半径（米）；a：轮子起始角度（度）；n：轨道数（条）；
;roll-m：轮子质量（千克）；track-m：轨道质量（千克）；ball-m：球质量（千克）。
(struct oaom-struct (ro ri a n roll-m track-m ball-m) #:mutable)
(define oaom-init (oaom-struct 1 0.1 15 5 10 1 10))

;求值间隔时间与旋转间隔：
;求之间隔时间需要和轮子旋转间隔作对比，如设定时间对应的旋转间隔大于设定值，则按旋转间隔设定值计算间隔时间，反之亦然。
(define DT 0.1) ;时间间隔，秒
(define DC 0.01) ;旋转间隔，米

;g的值：
(define g 9.8)

;=====================================================================
;设置oaom基础参数：
(define (set-oaom-init #:ro [ro 2] #:ri [ri 0.1] #:a [a 10] #:n [n 5]
                       #:roll-m [roll-m 10]
                       #:track-m [track-m 1]
                       #:ball-m [ball-m 3])
  (set-oaom-struct-ro! oaom-init ro)
  (set-oaom-struct-ri! oaom-init ri)
  (set-oaom-struct-a! oaom-init a)
  (set-oaom-struct-n! oaom-init n)
  (set-oaom-struct-roll-m! oaom-init roll-m)
  (set-oaom-struct-track-m! oaom-init track-m)
  (set-oaom-struct-ball-m! oaom-init ball-m))

;测试：
(module+ test
  (set-oaom-struct-ro! oaom-init 10)
  (oaom-struct-ro oaom-init)
  )

