#lang racket

(require "value.rkt"
         "ball.rkt"
         "M-class.rkt")

(provide track%)

(define/contract track%
  (class/c (init-field [at at?]
                       [lt +real?])
           [update-oaom-init (->m #:m +real? #:ri +real? any)]
           [update-field (->m real? any)])
  (class M-object%
    (super-new)
    
    (init-field at ;at：轨道角度，应在(0 <= at < 360)之间
                lt ;lt：轨道长度
                [m (oaom-struct-track-m oaom-init)] ;质量
                [ri (oaom-struct-ri oaom-init)]) ;轮子内圆半径

    (field [l (/ lt 2)] ;轨道中心点长度（半长）
           [aL (eval-aL at l ri)] ;力臂圆心角度
           [aF (eval-aF at (eval-af at) l ri)] ;球的力矩角
           [M (eval-M)]) ;力矩
    
    (inherit check-a
             eval-af
             eval-aF
             eval-aL
             F-flag)
    
    ;==============================================
    ;更新oaom初始字段值：
    (define/public (update-oaom-init #:m m-set #:ri ri-set)
      (set! m m-set)
      (set! ri ri-set))
    
    ;设置角度a（度）：
    (define/private (set-at a-track)
      (set! at a-track))

    ;设置M值：
    (define/private (set-M)
      (set! M (eval-M)))
    
    ;求取M值：
    (define/private (eval-M)
      (* (eval-F) (eval-L)))
    
    ;求值F的宏：
    (define-syntax-rule (eval-F)
      (* (F-flag aL)
         (* m g)
         (abs (sin (degrees->radians aF)))))

    ;设置aF字段（度）：
    (define/private (set-aF)
      (set! aF (eval-aF at (eval-af at) l ri)))

    ;设置aL字段（度）：
    (define/private (set-aL)
      (eval-aL at l ri))
    
    ;求取L的宏：
    (define-syntax-rule (eval-L)
      (sqrt (+ (* ri ri) (* l l))))

    ;更新字段：
    ;按这个顺序：at->aF->M
    (define/public (update-field a-track)
      (set-at a-track)
      (set-aL)
      (set-aF)
      (set-M))

    ;绘制轨道：
    (define/public (draw dc x-center y-center scale ro)
      (let ([p-start (eval-p-start)]
            [p-end (eval-p-end ro)])
        (send dc draw-line
              (+ x-center (* (car p-start) scale))
              (+ y-center (* (cadr p-start) scale))
              (+ x-center (* (car p-end) scale))
              (+ y-center (* (cadr p-end) scale)))))
    ;求值p-start：
    (define-syntax-rule (eval-p-start)
      (let ([ari (degrees->radians at)])
        (list (* ri (cos ari))
              (* ri (sin ari)))))
    ;求值p-end：
    (define-syntax-rule (eval-p-end ro)
      (let ([aro (eval-aro)])
        (list (* ro (cos aro))
              (* ro (sin aro)))))
    
    ;求取eval-aro（弧度）：
    (define-syntax-rule (eval-aro)
      (- (degrees->radians at)
         (atan (/ lt ri))))
    
    ;查看属性值：
    (define/public (format-property)
      (format "a：~a；l：~a；m：~a；M：~a。\n" at l m M))
    ))

;测试：=====================================================
(module+ test
(define track (new track% [at 30] [lt 1]))
(display
 (string-append
  (format "track%参数，\n")
  (send track format-property)
  "\n\n"))
  )
