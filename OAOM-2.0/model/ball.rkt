#lang racket

(require "value.rkt"
         "M-class.rkt")

(provide ball%)
  
(define/contract ball%
  (class/c (init-field [at at?]
                       [lt +real?])
           [update-oaom-init (->m #:m +real? #:ri +real? any)]
           [update-field (->m +real? real? any)])
  (class M-object%
    (super-new)

    (init-field at ;at：轨道角度，应在(0 <= at < 360)之间
                lt ;lt：轨道长度
                [m (oaom-struct-ball-m oaom-init)] ;球的质量.
                [ri (oaom-struct-ri oaom-init)]) ;轮子内圆半径

    (field [r 50] ;球半径（毫米）
           [af (eval-af at)] ;球沿轨道的受力角
           [am (eval-am)] ;加速度
           [v 0] ;当前沿轨道的速度，初始值为0
           [l (init-l)] ;在轨道上距中心端点的距离
           [aL (eval-aL at l ri)] ;力臂圆心角度
           [aF (eval-aF at af l ri)] ;球的力矩角
           [M (eval-M)]) ;力矩

    (inherit check-a
             eval-aF
             eval-af
             F-flag
             eval-aL)
    
    ;;=============================================
    ;更新oaom初始字段值：
    (define/public (update-oaom-init #:m m-set #:ri ri-set)
      (set! m m-set)
      (set! ri ri-set))
    
    ;设置at值（度）：
    (define/private (set-at a-track)
      (set! at a-track))

    ;设置af字段（度）：
    ;必须确保在初始化轨道时，不能让其角度大于或等与360度。
    (define/private (set-af)
      (set! af (eval-af at)))
    
    ;初始化l：
    (define/private (init-l)
      (cond
        [(or (= at 90) (= at 270)) ;轨道处于水平状态，
         (* (random) lt)] ;球位置随机
        [(and (> at 90) (< at 270)) lt]
        [else 0]))

    ;设置am:
    (define/private (set-am)
      (set! am (eval-am)))
    
    ;求取am：
    (define/private (eval-am)
        (* (am-flag)
           (* g (cos (degrees->radians af)))))

    ;定义am正负标志宏：
    (define-syntax-rule (am-flag)
      (cond
        [(or (< at 90) (> at 270)) -1] ;向内，加速度与设定方向相反
        [(and (> at 90) (< at 270)) 1] ;向外，加速度与设定方向同向
        [else 0])) ;水平方向，为0

    ;设置dt时间后的v值：
    (define/private (set-v dt)
      (set! v (eval-v dt)))
    
    ;求值dt时间后的v值：
    (define-syntax-rule (eval-v dt)
      (cond
        [(or (and (= l 0) (< am 0)) ;球在轮子内侧，加速度也向内侧
             (and (= l lt) (> am 0))) ;球在外侧，加速度也向外侧
         0] ;球的速度为0
        [else (+ v (* am dt))]))
 
    ;设置dt时间后的l值：
    (define/private (set-l dt)
      (set! l (eval-l dt)))
    
    ;求取dt时间后的l值：
    (define-syntax-rule (eval-l dt)
      (let ([l-dt (+ l (+ (* v dt) (* 1/2 am (* dt dt))))])
        (cond
          [(<= l-dt 0) 0] ;到内侧底
          [(>= l-dt lt) lt] ;到外侧底
          [else l-dt])))

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
      (set! aF (eval-aF aL af l ri)))

    ;设置aL字段（度）：
    (define/private (set-aL)
      (eval-aL at l ri))
    
    ;求取L的宏：
    (define-syntax-rule (eval-L)
      (sqrt (+ (* ri ri) (* l l))))

    ;更新dt时间后的字段值：
    ;求值顺序：l->v->at->af->am->aL->aF->M。
    (define/public (update-field dt a-track)
      (set-l dt)
      (set-v dt)
      (set-at a-track)
      (set-af)
      (set-am)
      (set-aL)
      (set-aF)
      (set-M))

    ;绘制球：
    (define/public (draw dc x-center y-center scale)
      (let ([bx-center (car (eval-ball-center))]
            [by-center (cadr (eval-ball-center))]
            [r-scale (* (/ r 1000) scale)])
        (send dc draw-ellipse
              (- (+ x-center (* bx-center scale)) r-scale)
              (- (+ y-center (* by-center scale)) r-scale)
              (* 2 r-scale)
              (* 2 r-scale))))
    
    ;求值球心点：
    (define-syntax-rule (eval-ball-center)
      (let ([a-ball (eval-a-ball)]
            [r-ball (eval-r-ball)])
        (list (* r-ball (cos a-ball))
              (* r-ball (sin a-ball)))))
    
    ;求取eval-a-ball：
    (define-syntax-rule (eval-a-ball)
      (- (degrees->radians at)
         (atan (/ l ri))))
    
    ;求取eval-r-ball：
    (define-syntax-rule (eval-r-ball)
      (sqrt (+ (* ri ri) (* l l))))
          
    ;查看属性值：
    (define/public (format-property)
      (format "v：~a；l：~a；af：~a；aL：~a；M：~a；am：~a。\n" v l af aL M am))
    ))

;测试：===================================================
#|
(module+ test
  ;ball在270-90之间：
  (define ball-1 (new ball% [at 20] [lt 1]))
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
|#

(module+ test
  ;0-90度之间：
  (define ball-4 (new ball% [at 20] [lt 1]))
  (display (send ball-4 format-property))
  (send ball-4 update-field 5 125)
  (display (send ball-4 format-property))
  (send ball-4 update-field 5 205)
  (display (send ball-4 format-property))
  (send ball-4 update-field 5 285)
  (display (send ball-4 format-property))
  )
