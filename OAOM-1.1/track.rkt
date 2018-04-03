#lang racket

(require "value.rkt"
         "ball.rkt")

(provide track%)

(define track%
  (class object%
    (super-new)
    
    (init-field at ;at：轨道角度，应在(0 <= at < 360)之间
                lt ;lt：轨道长度
                [m (oaom-struct-track-m oaom-init)] ;质量
                [ri (oaom-struct-ri oaom-init)]) ;轮子内圆半径

    (field [l (/ lt 2)] ;轨道中心点长度（半长）
           [aF (eval-aF ri)] ;球的力矩角
           [M (eval-M ri)]) ;力矩
    
    ;==============================================
    ;更新oaom初始字段值：
    (define/public (update-oaom-init #:m m-set #:ri ri-set)
      (set! m m-set)
      (set! ri ri-set))
    
    ;设置角度a：
    (define/private (set-at dta)
      (set! at (check-at (+ at dta))))

    ;检查a值，确保在360以内：
    (define/private (check-at a)
      (let ([ac (- a (* (quotient (truncate a) 360) 360))])
        (if (>= ac 0)
            ac
            (+ ac 360))))
       
    ;求取M值：
    (define/private (eval-M ri)
      (* (a-F-flag at)
         (eval-F ri)
         (eval-L ri)))
    
    ;求值F的宏：
    (define-syntax-rule (eval-F ri)
      (* (* m g)
         (abs (sin (eval-aF ri)))))
    
    ;求值aF的宏：
    (define-syntax-rule (eval-aF ri)
      (+ (degrees->radians at)
         (* (aF-flag) (atan (/ ri l)))))
    
    ;求值aF-flag的宏：
    (define-syntax-rule (aF-flag)
      (cond
        [(or (and (> at 0) (< at 90))
             (and (> at 180) (< at 270))) 1]
        [(or (and (>= at 90) (< at 180))
             (and (>= at 270) (< at 360))) -1]
        [else 0]))
    
    ;设置aF字段：
    (define/private (set-aF ri)
      (set! aF (eval-aF ri)))
    
    ;求取L的宏：
    (define-syntax-rule (eval-L ri)
      (sqrt (+ (* ri ri) (* l l))))
    
    ;定义M正负标志宏：
    (define-syntax-rule (a-F-flag a)
      (cond
        [(and (> a 180) (< a 360)) -1] ;加速度与设定方向相反
        [(and (> a 0) (< a 180)) 1] ;同向
        [else 0])) ;垂直方向，为0
    
    ;设置M值：
    (define/private (set-M ri)
      (set! M (eval-M ri)))

    ;更新字段：
    (define/public (update-field dta)
      (set-at dta)
      (set-aF ri)
      (set-M ri))
    
    ;查看属性值：
    (define/public (format-property)
      (format "a：~a；l：~a；m：~a；M：~a。\n" at l m M))
    ))

;测试：==============================
(module+ test
(define track (new track% [at 30] [lt 1]))
(display
 (string-append
  (format "track%参数，\n")
  (send track format-property)
  "\n\n"))
  )
