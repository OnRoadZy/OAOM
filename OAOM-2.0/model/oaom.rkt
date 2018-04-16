#lang racket

(require "roll.rkt"
         "track.rkt"
         "ball.rkt"
         "value.rkt")

;更新oaom基础参数：
;roll必须是roll%对象。
(define (update-oaom-init roll)
  ;更新roll参数：
  (send roll reset-roll
        #:ro (oaom-struct-ro oaom-init)
        #:ri (oaom-struct-ri oaom-init)
        #:m (oaom-struct-roll-m oaom-init)
        #:a (oaom-struct-a oaom-init)
        #:n (oaom-struct-n oaom-init))
  ;更新track/balls参数：
  (for/vector ([tb (send roll get-track/balls)])
    (let ([track (track/ball-struct-track tb)]
          [ball  (track/ball-struct-ball tb)])
      (begin
        (send track update-oaom-init
              #:m (oaom-struct-track-m oaom-init)
              #:ri (oaom-struct-ri oaom-init))
        (send ball update-oaom-init
              #:m (oaom-struct-track-m oaom-init)
              #:ri (oaom-struct-ri oaom-init))))))

;=====================================================
;测试：
(module+ test
;定义roll对象：
  (define roll (new roll%))
  ;(set-oaom-init)
  (set-oaom-init #:ro 1.3 #:ri 0.1 #:a 20 #:n 7
                 #:roll-m 0
                 #:track-m 0.01
                 #:ball-m 1)
  (reset-roll roll)
  (send roll run 100)
  )


