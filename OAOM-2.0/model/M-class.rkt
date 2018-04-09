#lang racket

(require "value.rkt")

(provide M-object%)

;===============================================
;建立一个通用化的基类，以将力矩相关的函数统一使用。
(define/contract M-object%
  (class/c [check-a (->m +real? at?)]
           [eval-at (->m +integer? at? +integer? real?)]
           [eval-aF (->m at? at? +real? +real? at?)]
           [eval-aL (->m at? +real? +real? at?)]
           [eval-af (->m at? at?)]
           [F-flag (->m +real? flag?)])
  (class object%
    (super-new)

    ;求取指定轨道的角度（度）：
    ;第一个轨道在a=0时初始位置设定为在轮子上方平行于水平方向。
    (define/public (eval-at i a n)
      (let ([a-track (+ a (* (/ 360 n) i))])
        (check-a a-track)))
    
   ;检查a值，确保在360以内（度）：
    (define/public (check-a a)
      (let ([ac (- a (* (quotient (truncate a) 360) 360))])
        (if (>= ac 0)
            ac
            (+ ac 360))))

    ;求值aF（度）：
    ;lg：受力点距轨道圆心端点的长度。
    (define/public (eval-aF a-track a-f lg ri-roll)
      (let ([atan-ri/lg
             (if (= lg 0) ;lg为0时，球在内圆位置
                 90
                 (radians->degrees (atan (/ ri-roll lg))))])
        (cond
          [(or (and (>= a-track 0) (< a-track 90))
               (and (> a-track 180) (< a-track 270))) (- a-f atan-ri/lg)]
          [(and (> a-track 90) (< a-track 180)) (- 90 a-f atan-ri/lg)]
          [(and (> a-track 270) (< a-track 360)) (+ a-f atan-ri/lg)]
          [(or (= a-track 0) (= a-track 180)) atan-ri/lg]
          [(or (= a-track 90) (= a-track 270)) (- 90 atan-ri/lg)])))

    ;求值aL（度）：
    (define/public (eval-aL a-track l ri)
      (let ([a-c (+ a-track
                    (radians->degrees (atan (/ l ri))))])
        (check-a a-c)))

    ;求取af（度）：
    (define/public (eval-af a-track)
      (cond ;必须确保在初始化轨道时，不能让其角度大于过等于360。
        [(and (>= a-track 0) (<= a-track 90)) a-track]
        [(and (> a-track 90) (<= a-track 180)) (- 180 a-track)]
        [(and (> a-track 180) (<= a-track 270)) (- a-track 180)]
        [(and (> a-track 270) (< a-track 360)) (- 360 a-track)]
        [else (display
               (format "a-track值~a有误（ball类eval-af函数报错。）" a-track))]))

    ;求值F-flag：
    (define/public (F-flag a-L)
      (cond
        [(or (and (>= a-L 0) (< a-L 90))
             (and (> a-L 270) (< a-L 360))) 1]
        [(and (> a-L 90) (< a-L 270)) -1]
        [else 0]))
   ))