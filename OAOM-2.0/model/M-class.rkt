#lang racket

(provide M-object%
         +real/c
         +integer/c
         at?)

;合约判断函数：===================================
;>=0的real值:
(define (+real/c v) (and (real? v) (>= v 0)))
;>=0的integer值:
(define (+integer/c v) (and (integer? v) (>= v 0)))
;at的值：
(define (at? a)
  (and (real? a) (>= a 0) (< a 360)))

;===============================================
;建立一个通用化的基类，以将力矩相关的函数统一使用。
(define/contract M-object%
  (class/c [check-at (->m +real/c +real/c)])
  (class object%
    (super-new)

    ;求取指定轨道的角度（度）：
    ;第一个轨道在a=0时初始位置设定为在轮子上方平行于水平方向。
    (define/public (eval-at i a n)
      (let ([a-track (+ a 90 (* (/ 360 n) i))])
        (check-at a-track)))
    
   ;检查a值，确保在360以内：
    (define/public (check-at a)
      (let ([ac (- a (* (quotient (truncate a) 360) 360))])
        (if (>= ac 0)
            ac
            (+ ac 360))))
   ))