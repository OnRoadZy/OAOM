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
  (class/c (init-field [ro +real?] [ri +real?]
                       [m +real?] [n +integer?]
                       [da +real?] [a +real?])
           (field [track/balls vector?])
           [reset-oaom (->m any)]
           [update-oaom (->m any)])
  
  (class M-object%
    (super-new)
    
    (init-field [ro (oaom-struct-ro oaom-init)];轮子外圆半径
                [ri (oaom-struct-ri oaom-init)];轮子内圆半径
                [m (oaom-struct-roll-m oaom-init)];质量
                [a (oaom-struct-a oaom-init)];初始角度
                [n (oaom-struct-n oaom-init)]);轨道个数
    
    (field [track/balls (create-track/balls)];轨道向量
           [w 0] ;角速度
           [M (eval-track/balls-M)] ;力矩
           [aw (eval-aw)] ;角加速度
           [dt (eval-dt)] ;与下一次计算的间隔时间
           [da (eval-dw dt)]) ;dt时间间隔内的角度变化

    (inherit eval-at
             check-a)
    
;============================================================
    ;重置轮子参数：
    (define/private (reset-roll)
      (set! ro (oaom-struct-ro oaom-init))
      (set! ri (oaom-struct-ri oaom-init))
      (set! m (oaom-struct-roll-m oaom-init))
      (set! a (oaom-struct-a oaom-init))
      (set! da 0)
      (set! n (oaom-struct-n oaom-init))
      (set! w 0)
      (set! M (eval-track/balls-M))
      (set! aw (eval-aw))
      (set! dt (eval-dt)))
    
    ;更新dt时间后的oaom初始字段值：
    (define/private (update-roll)
      ;按以下顺序求字段值(aw->dt->da->a2->w2->v2->M2)：
      (set-aw) (set-dt) (set-da) (set-a) (set-w) (set-M)
      (update-track/balls))

    ;设置轨道向量表：
    (define/private (set-track/balls)
      (set! track/balls (create-track/balls)))
    
    ;创建轨道向量表：
    (define/private (create-track/balls)
      (let ([tbs (make-vector n)] ;轨道球结构向量表
            [l-track (eval-lt)]) ;轨道长度
        (begin
          (for ([i n])
            (let ([a-track (eval-at i a n)])
              (vector-set! tbs i
                           (track/ball-struct
                            (new track% [at a-track] [lt l-track])
                            (new ball% [at a-track] [lt l-track])))))
          tbs)))
    
    ;求取轨道长度（米）：
    (define-syntax-rule (eval-lt)
      (sqrt (- (* ro ro) (* ri ri))))

    ;求值w（弧度/秒）：
    (define/private (set-w)
      (set! w (+ w (* 1/2 aw dt))))

    ;设置字段aw：
    (define/private (set-aw)
      (set! aw (eval-aw)))
    
    (define-syntax-rule (eval-aw)
      (/ M (+ (* n (oaom-struct-ball-m oaom-init))
              (* n (oaom-struct-track-m oaom-init))
              m)))

    ;设置轮子起始角a（度）：
    (define/private (set-a)
      (set! a
            (check-a
             (- a (radians->degrees
                   (eval-dw dt))))))

    ;设置da（度）：
    (define/private (set-da)
      (set! da (radians->degrees (eval-dw dt))))

    ;求值a的宏（弧度）：
    (define-syntax-rule (eval-dw dlt)
       (+ (* w dlt) (* 1/2 aw (* dlt dlt))))

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
      (let ([M-c (eval-track/balls-M)])
        (set! M
              (if (> MX 0)
                  (if (> M-c 0)
                      (if (> M-c MX) (- M-c MX) 0)
                      (if (> (abs M-c) MX) (+ M-c MX) 0))
                  M-c))))
    
    ;设置间隔时间dt字段：
    (define/private (set-dt)
      (eval-dt))

    ;求值dt：
    (define/public (eval-dt)
      (let ([dw-c (/ (/ DC 1000) ro)])
        ;按预定间隔时间计算旋转角大于预定值
        (if (<= (abs (eval-dw DT)) dw-c)
            DT ;使用预定时间间隔
            (let ([a-dt (abs (* 1/2 aw))]
                  [b-dt (abs w)]
                  [c-dt (- dw-c)])
              (/ (- (sqrt (- (* b-dt b-dt)
                             (* 4 a-dt c-dt)))
                    b-dt)
                 (* 2 a-dt))) ;以预定旋转间隔重新计算dt值
            )))

    ;计算总质量：
    (define/private (total-m)
      (let ([mb (* n (oaom-struct-ball-m oaom-init))]
            [mt (* n (oaom-struct-track-m oaom-init))]
            [mr m])
        (+ mb mt mr)))

    ;更新track/balls字段：
    (define/private (update-track/balls)
        (for/vector ([tb track/balls]
                     [i (range n)])
          (let ([track (track/ball-struct-track tb)]
                [ball (track/ball-struct-ball tb)]
                [a-track (eval-at i a n)])
            (send track update-field a-track)
            (send ball update-field dt a-track))))

    ;重置oaom模型：
    (define/public (reset-oaom)
      ;设置roll参数：
      (reset-roll)
      ;设置track/balls参数：
      (set-track/balls))

    ;更新oaom模型：
    (define/public (update-oaom)
      ;更新轮子参数：
      (update-roll)
      ;更新track/balls参数：
      (update-track/balls))
    
    ;绘制轮子：
    (define/public (draw dc w-canvas h-canvas)
      (let ([scale (eval-scale w-canvas h-canvas)]
            [x-center (/ w-canvas 2)]
            [y-center (/ h-canvas 2)])
        (begin
          ;绘制大圆：
          (send dc draw-ellipse
                (- x-center (* ro scale)) (- y-center (* ro scale))
                (* ro 2 scale) (* ro 2 scale))
          ;绘制小圆：
          (send dc draw-ellipse
                (- x-center (* ri scale)) (- y-center (* ri scale))
                (* ri 2 scale) (* ri 2 scale))
          ;绘制轨道和球：
          (for/vector ([tb track/balls])
            (let ([track (track/ball-struct-track tb)]
                  [ball (track/ball-struct-ball tb)])
              (begin
                (send track draw dc x-center y-center scale ro)
                (send ball draw dc x-center y-center scale))))
          )))

    ;计算绘图缩放比例：
    (define-syntax-rule (eval-scale w h)
      (if (> w h)
          (/ h (* 2 ro))
          (/ w (* 2 ro))))
       
    ;旋转轮子：
    ;即，求值轮子在dlt时间内或c旋转角后（取决于判断）的参数值。
    (define/public (run times)
      (reset-roll)
      (for ([i times])
        (update-roll)
        (display (format-main-property)))) ;显示变化情况
    
    ;查看全部属性值：
    (define/public (format-all-property)
      (format "ro：~a；ri：~a；m：~a；n：~a；\ndt：~a；w：~a；a：~a；M：~a；\ntrack/balls：~a。\n"
              ro ri m n
              dt w a M
              track/balls))

    ;查看主要属性值：
    (define/public (format-main-property)
      (format "ro：~a；ri：~a；m：~a；n：~a；\ndt：~a；w：~a；a：~a；M：~a。\n"
              ro ri m n
              dt w a M))
    
    ;查看轨道球体向量内容：
    (define/public (format-track/ball)
      (let ([str ""])
        (for/vector ([tb track/balls])
          (let ([track (track/ball-struct-track tb)]
                [ball (track/ball-struct-ball tb)])
            (begin
              (string-append str
                             (format "~a\n~a\n"
                                     (send track format-property)
                                     (send ball format-property))))))))
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
#|
(module+ test
  (send roll run 10))
|#
#|
(module+ test
  (set-oaom-init #:ro 1.3 #:ri 0.2 #:a 0 #:n 1
                       #:roll-m 1
                       #:track-m 1
                       #:ball-m 5)
  (send roll run 100))
|#
(module+ test
  (set-oaom-init #:ro 1.3 #:ri 0.2 #:a 15 #:n 10
                       #:roll-m 1
                       #:track-m 1
                       #:ball-m 10)
  (send roll run 100))