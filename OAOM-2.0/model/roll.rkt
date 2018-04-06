#lang racket

(require "ball.rkt"
         "track.rkt"
         "value.rkt"
         "M-class.rkt")

(provide roll%
         (struct-out track/ball-struct))

;创建track/ball-struct：
(struct track/ball-struct (track ball))
  
(define/contract roll%
  (class/c (init-field [ro +real/c] [ri +real/c]
                       [m +real/c] [a +integer/c] [n +integer/c])
           (field [track/balls vector?])
           [update-oaom-init (->m #:ro +real/c #:ri +real/c
                                  #:m +real/c #:a +integer/c #:n +integer/c
                                  any)]
           [get-track/balls (->m vector?)]
           [update-field (->m +real/c any)]
           [run (->m +integer/c any)])
  
  (class M-object%
    (super-new)
    
    (init-field [ro (oaom-struct-ro oaom-init)];轮子外圆半径
                [ri (oaom-struct-ri oaom-init)];轮子内圆半径
                [m (oaom-struct-roll-m oaom-init)];质量
                [a (oaom-struct-a oaom-init)];初始角度
                [n (oaom-struct-n oaom-init)]);轨道个数
    
    (field [track/balls (create-track/balls)];轨道向量
           [w 0] ;角速度
           [v 0] ;线速度
           [M (eval-track/balls-M)]) ;力矩

    (inherit check-at)
    
    ;============================================================
    ;更新oaom初始字段值：
    (define/public (update-oaom-init #:ro ro-set #:ri ri-set #:m m-set #:a a-set #:n n-set)
      (set! ro ro-set)
      (set! ri ri-set)
      (set! m m-set)
      (set! a a-set)
      (set! n n-set))
     
    ;创建轨道向量表：
    (define/private (create-track/balls)
      (let ([tbs (make-vector n)] ;轨道球结构向量表
            [l-track (eval-l)]) ;轨道长度
        (begin
          (for ([i n])
            (let ([a-track (eval-at i)])
              (vector-set! tbs i
                           (track/ball-struct
                            (new track% [at a-track] [lt l-track])
                            (new ball% [at a-track] [lt l-track])))))
          tbs)))
    
    ;取得轨道向量表：
    (define/public (get-track/balls)
      track/balls)
    
    ;求取指定轨道的角度（度）：
    ;第一个轨道在a=0时初始位置设定为在轮子上方平行于水平方向。
    (define/private (eval-at i)
      (let ([a-track (+ a 90 (* (/ 360 n) i))])
        (check-at a-track)))

    ;求取轨道长度（米）：
    (define-syntax-rule (eval-l)
      (sqrt (- (* ro ro) (* ri ri))))

    ;求值w：
    (define/private (set-w dt)
      (set! w (+ w (* 1/2 (eval-aw) dt))))
    
    (define-syntax-rule (eval-aw)
      (/ M (+ (* n (oaom-struct-ball-m oaom-init))
              (* n (oaom-struct-track-m oaom-init))
              m)))

    ;求值线速度v：
    (define/private (set-v)
      (set! v (* w ro)))

    ;求值力矩M：
    (define/private (eval-track/balls-M)
      (let ([total-M 0])
        (for/vector ([tb track/balls])
          (let ([track (track/ball-struct-track tb)]
                [ball (track/ball-struct-ball tb)])
            (set! total-M (+ (get-field M track)
                             (get-field M ball)))))
        total-M))
    
    ;设置合力矩：
    (define/private (set-M)
      (set! M (eval-track/balls-M)))
    
    ;求值dt时间内转过的圆周长c：
    (define/private (eval-c dt)
      (* v dt))
    
    ;求值dt：
    (define/private (eval-dt)
      (if (> (eval-c DT) DC) ;按预定间隔时间计算旋转圆周间隔大于预定值
          (/ DC v) ;以预定旋转间隔重新计算dt值
          DT)) ;使用预定时间间隔

    ;求值dta（dt时间后轮子的旋转角）,单位：度：
    (define/private (eval-dta dt)
      (radians->degrees (* w dt)))

    ;计算总质量：
    (define/private (total-m)
      (let ([mb (* n (oaom-struct-ball-m oaom-init))]
            [mt (* n (oaom-struct-track-m oaom-init))]
            [mr m])
        (+ mb mt mr)))

    ;更新字段：
    (define/public (update-field dt)
      (set-w dt)
      (set-v)
      (update-field-track/balls dt)
      (set-M))

    ;更新track/balls字段：
    (define/private (update-field-track/balls dt)
      (let ([dta (eval-dta dt)])
      (for/vector ([tb track/balls])
        (let ([track (track/ball-struct-track tb)]
              [ball (track/ball-struct-ball tb)])
          (send track update-field dta)
          (send ball update-field dt dta)))))

    ;旋转轮子：
    ;即，求值轮子在dt时间内或c旋转角后（取决于判断）的参数值。
    (define/public (run times)
      (for ([i times])
        (let ([dt (eval-dt)])
          (update-field dt)
          (display (format-property))))) ;显示变化情况
    
    ;查看属性值：
    (define/public (format-property)
      (format "ro：~a；ri：~a；m：~a；n：~a；\ndt：~a；w：~a；v：~a；M：~a；\ntrack/balls：~a。\n"
              ro ri m n
              (eval-dt) w v M
              track/balls))
    
    ;查看轨道球体向量内容：
    (define/public (format-track/ball)
      (let ([str ""])
        (for/vector ([tb track/balls])
          (let ([track (track/ball-struct-track tb)]
                [ball (track/ball-struct-ball tb)])
            (begin
              (string-append str
                             (format "~a\n~a\n"
                                     (send track get-property)
                                     (send ball get-property))))))))
    ))

;测试：====================================================================
(module+ test
  (define roll (new roll%)))

#|
(module+ test
  (display
   (string-append
    (format "track%参数，\n")
    (send roll format-property)
    "\n\n")))
|#
#|
(module+ test
  (display (send roll format-track/ball)))
|#

(module+ test
  (send roll run 10))