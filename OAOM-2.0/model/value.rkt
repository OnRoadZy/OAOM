#lang racket

(provide (struct-out oaom-struct)
         g
         DT DC
         MX)

(provide +real?
         +integer?
         at?
         flag?)

(provide (contract-out
          [oaom-init (struct/c oaom-struct
                               real? real? integer? integer?
                               real? real? real?)]
          [set-oaom-init (-> #:ro real? #:ri real? #:a integer? #:n integer?
                             #:roll-m real?
                             #:track-m real?
                             #:ball-m real?
                             any )]
          [set-DC (-> +real? any)]
          [set-DT (-> +real? any)]))

;系统运行参数=================================================
;ro：外圆半径(米)；ri：内圆半径（米）；a：轮子起始角度（度）；n：轨道数（条）；
;roll-m：轮子质量（千克）；track-m：轨道质量（千克）；ball-m：球质量（千克）。
(struct oaom-struct (ro ri a n roll-m track-m ball-m) #:mutable)
(define oaom-init (oaom-struct 1.3 0.2 15 7 1 1 10))

;求值间隔时间与旋转间隔：
;求之间隔时间需要和轮子旋转间隔作对比，如设定时间对应的旋转间隔大于设定值，则按旋转间隔设定值计算间隔时间，反之亦然。
(define DT 0.01) ;时间间隔，秒
(define DC 10) ;旋转间隔，毫米

;g的值：
(define g 9.8)

;消耗力矩：
;如轴承等产生的反力矩。
(define MX 0)

;公共函数==================================================
;设置oaom基础参数：
(define (set-oaom-init #:ro ro #:ri ri #:a a #:n n
                       #:roll-m roll-m
                       #:track-m track-m
                       #:ball-m ball-m)
  (set-oaom-struct-ro! oaom-init ro)
  (set-oaom-struct-ri! oaom-init ri)
  (set-oaom-struct-a! oaom-init a)
  (set-oaom-struct-n! oaom-init n)
  (set-oaom-struct-roll-m! oaom-init roll-m)
  (set-oaom-struct-track-m! oaom-init track-m)
  (set-oaom-struct-ball-m! oaom-init ball-m))

;设置DC：
(define (set-DC dc)
  (set! DC dc))

;设置DT：
(define (set-DT dt)
  (set! DT dt))

;合约判断函数：========================================
;>=0的real值:
(define (+real? v) (and (real? v) (>= v 0)))
;>=0的integer值:
(define (+integer? v) (and (integer? v) (>= v 0)))
;at的值：
(define (at? v)
  (and (real? v) (>= v 0) (< v 360)))
;flag标志：
(define (flag? v)
  (and (integer? v)
       (or (= v -1) (= v 1) (= v 0))))

;测试：=================================================================
(module+ test
  (set-oaom-struct-ro! oaom-init 10)
  (oaom-struct-ro oaom-init)
  )

