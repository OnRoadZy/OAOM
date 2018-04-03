#lang racket

(require "value.rkt"
         "M-class.rkt")

(provide ball%)
  
(define/contract ball%
  (class/c (init-field [at at?]
                       [lt +real/c])
           [update-oaom-init (->m #:m +real/c #:ri +real/c any)]
           [update-field (->m +real/c real? any)])
  (class M-object%
    (super-new)

    (init-field at ;at：轨道角度，应在(0 <= at < 360)之间
                lt ;lt：轨道长度
                [m (oaom-struct-ball-m oaom-init)] ;球的质量.
                [ri (oaom-struct-ri oaom-init)]) ;轮子内圆半径

    (field [af (eval-af)] ;球沿轨道的受力角
           [am (eval-am)] ;加速度
           [v 0] ;当前沿轨道的速度，初始值为0
           [l (init-l)] ;在轨道上距中心端点的距离
           [aF (eval-aF ri)] ;球的力矩角
           [M (eval-M ri)]) ;力矩

    (inherit check-at)
    
    ;;=============================================
    ;更新oaom初始字段值：
    (define/public (update-oaom-init #:m m-set #:ri ri-set)
      (set! m m-set)
      (set! ri ri-set))
    
    ;设置at值：
    (define/private (set-at dta)
      (set! at (check-at (+ at dta))))
   
    ;求取af的宏：
    (define-syntax-rule (eval-af)
      (cond ;必须确保在初始化轨道时，不能让其角度大于过等于360
        [(and (>= at 0) (<= at 90)) at]
        [(and (> at 90) (<= at 180)) (- at 90)]
        [(and (> at 180) (<= at 270)) (- at 180)]
        [(and (> at 270) (< at 360)) (- at 270)]
        [else (display
               (format "at值~a有误（ball类eval-af函数报错。）" at))]))

    ;设置af字段：
    (define/private (set-af)
      (set! af (eval-af)))
    
    ;初始化l：
    (define/private (init-l)
      (cond
        [(or (= at 90) (= at 270)) ;轨道处于水平状态，
         (* (random) lt)] ;球位置随机
        [(and (> at 90) (< at 270)) 0]
        [else lt]))

    ;求取am：
    (define/private (eval-am)
      (let ([ad (a-am-flag)])
        (* ad (* g (cos (degrees->radians af))))))

    ;定义am正负标志宏：
    (define-syntax-rule (a-am-flag)
      (cond
        [(and (> at 90) (< at 270)) -1] ;加速度与设定方向相反
        [(or (< at 90) (> at 270)) 1] ;同向
        [else 0])) ;水平方向，为0

    (define/private (set-am)
      (set! am (eval-am)))
    
    ;设置v：
    (define/private (set-v dt)
      (set! v (eval-v dt)))
    
    ;求值v：
    (define-syntax-rule (eval-v dt)
      (cond
        [(or (and (= l 0) (< am 0)) ;球在轮子内侧，加速度也向内侧
             (and (= l lt) (> am 0))) ;球在外侧，加速度也向外侧
         0] ;球的速度为0
        [else (+ v (* am dt))]))
 
    ;设置l：
    (define/private (set-l dt)
      (set! l (eval-l dt)))
    
    ;求取l：
    (define-syntax-rule (eval-l dt)
      (let ([cl (+ l (* (+ v (* am dt)) dt))])
        (cond
          [(<= cl 0) 0] ;到内侧底
          [(>= cl lt) lt] ;到外侧底
          [else cl])))

    ;求取M值：
    (define/private (eval-M ri)
      (* (a-F-flag) (eval-F ri) (eval-L ri)))
    
    ;求值F的宏：
    (define-syntax-rule (eval-F ri)
      (* (* m g)
         (abs (sin (eval-aF ri)))))
    
    ;求值aF的宏：
    (define-syntax-rule (eval-aF ri)
      (if (= l 0) ;l为0时，球在内圆位置
          (- (/ 2 pi) (degrees->radians at))
          (+ (degrees->radians at)
             (* (aF-flag) (atan (/ ri l))))))
    
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
    (define-syntax-rule (a-F-flag)
      (cond
        [(and (> at 180) (< at 360)) -1] ;加速度与设定方向相反
        [(and (> at 0) (< at 180)) 1] ;同向
        [else 0])) ;垂直方向，为0
    
    ;设置M值：
    (define/private (set-M ri)
      (set! M (eval-M ri)))
     
    ;更新字段：
    (define/public (update-field dt dta)
      (set-at dta)
      (set-af)
      (set-am)
      (set-v dt)
      (set-l dt)
      (set-aF ri)
      (set-M ri))
          
    ;查看属性值：
    (define/public (format-property)
      (format "v：~a；l：~a；af：~a；M：~a；am：~a。\n" v l af M am))
    ))

;测试：===================================================
(module+ test
  ;ball在270-90之间：
  (define ball-1 (new ball% [at 20] [lt 1]))
  (display
   (string-append
    (format "ball在270-90之间，\n")
    (send ball-1 format-property)
    (format "设定时间后，\n")
    (begin
      (send ball-1 update-field 5 30)
      (send ball-1 format-property))
    "\n\n"))
  )

(module+ test
  ;ball在90-270之间：
  (define ball-2 (new ball% [at 120] [lt 1]))
  (display
   (string-append
    (format "ball在90-270之间，\n")
    (send ball-2 format-property)
    (format "设定时间后，\n")
    (begin
      (send ball-2 update-field 5 125)
      (send ball-2 format-property))
    "\n\n"))
  )

(module+ test
  ;ball在90或270：
  (define ball-3 (new ball% [at 90] [lt 1]))
  (display
   (string-append
    (format "ball在90或270，\n")
    (send ball-3 format-property)
    (format "设定时间后，\n")
    (begin
      (send ball-3 update-field 5 95)
      (send ball-3 format-property))
    (format "设定时间后，\n")
    (begin
      (send ball-3 update-field 5 95)
      (send ball-3 format-property))
    "\n\n"))
  )