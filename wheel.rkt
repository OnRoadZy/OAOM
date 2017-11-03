;#lang racket
(module wheel racket
  (provide wheel%)

  (require "track.rkt" "ball.rkt")
  
  (define wheel%
    (class object%
      ;;初始化转轮参数：
      (init [R0 100];圆盘半径m。
            [m0 100];圆盘质量Kg。
            [nt0 7];轨道数。
            [dr0 10];第一个轨道的近圆心端点起始角度(度)。
            [d0-int 5];计算的转动角度间隔。
            [R0-track 100];轨道外圆半径（m）。
            [r0-track 20];轨道内圆半径（m）。
            [m0-track 10];轨道质量（Kg）。
            [m0-ball 10]);重力球质量（Kg）。

      (super-new)
      
      ;;圆盘参数：
      (field [R R0];圆盘半径R(m)。
             [m m0];圆盘质量m(Kg)。
             [nt nt0];轨道组数nt。
             [cur-dr0 dr0];第一个轨道的近圆心端点角度cur-dr(度)。
             [d-int d0-int];计算的转动角度间隔d-int（度）。
             [R-track R0-track];轨道外圆半径R-track（m）。
             [r-track r0-track];轨道内圆半径r-track（m）。
             [m-track m0-track];轨道质量m-track（Kg）。
             [m-ball m0-ball];重力球质量m-ball（Kg）。
             [tracks (init-tracks)];轨道对象表tracks。
             [cur-M (cal-M)];当前圆盘合力矩M(N/m)。
             [cur-w (cal-w)];当前圆盘角加速度w（弧度/s^2）。
             [cur-vw 0];当前圆盘角速度vw（弧度/s）。
             [cur-dt 0]);当前转过d-int所用时间（s）。
      
      ;;初始化轨道对象向量表：
      (define/private (init-track n);初始化第n条轨道。
        (new track%
             [R0 R-track]
             [r0 r-track]
             [m0-ball m-ball]
             [m0 m-track]
             [d0 (cal-track-d n)]))
      (define/private (init-tracks);初始化轨道表。
        (let ([tks (make-vector nt)])
          (for ([i nt])
            (vector-set! tks i (init-track i)))
          tks))
      ;计算轨道角度：
      (define/private (cal-track-d n);计算第n个球的轨道角度。
         (+ (cal-dr n) 90))
      (define/private (cal-dr n);计算第n个球轨道的近圆心端点起始角度。
        (+ cur-dr0 (* n (cal-gap-d))))
      (define/private (cal-gap-d);计算各轨道之间的间隔角度。
        (/ 360 nt))
      
      ;计算并设置圆盘力矩：
      (define/private (cal-M);合计圆盘各轨道产生的力矩。
        (for/sum ([i nt])
          (send (vector-ref tracks i) get-M)))
      (define/private (set-M);设置圆盘当前力矩。
        (set! cur-M (cal-M))
        (printf "\n圆盘合力矩：~a" (get-M)))
      (define/public (get-M);取得圆盘当前力矩。
        cur-M)
      
      ;;计算圆盘角加速度：
      (define/private (cal-w);计算圆盘角加速度w。
        (/ (get-M) (cal-m)))
      (define/private (set-w);设置当前角加速度。
        (set! cur-w (cal-w))
        (printf "\n圆盘角加速度：~a" (get-w)))
      (define/public (get-w);取得当前角加速度。
        cur-w)
      (define/private (cal-m);计算整体质量总和。
        (+ m
           (cal-m-tracks)))
      (define/private (cal-m-tracks);计算轨道质量总和。
        (for/sum ([i nt])
            (send (vector-ref tracks i) cal-m)))

      ;;计算圆盘角速度vw:
      (define/private (cal-vw dt);计算圆盘dt时间后的角速度vw。
        (+ cur-vw (* 1/2 cur-w dt)))
      (define/private (set-vw dt);设置dt时间后的角速度。
        (set! cur-vw (cal-vw dt))
        (printf "\n圆盘角速度：~a" cur-vw))
      (define/public (get-vw);取得当前角速度。
        cur-vw)

      ;;计算转动d-int所耗时间：
      (define/private (cal-dt);计算转动d-int角度所费时间dt。
        ;x=[-b±sqrt(b^2-4ac)]/2a 一元二次方程求根公式。
        ;dt={-vw±sqrt[vw*vw -4*(w/2)*(- dint)]}*[(1/2)*(w/2)
        ;  =[-vw±sqrt(vw*vw+2*w*dint)]*w/4 
        (let ([dint d-int])
          ;确保旋转角度与角速度方向一致:
          (cond [(< cur-vw 0)
                 (set! dint (- dint))]
                [(= cur-vw 0)
                 (when (< cur-w 0)
                   (set! dint (- dint)))])
          (set! dint (degrees->radians dint));进行角度到弧度的转换。
          (if (= cur-w 0)
              (if (= cur-vw 0)
                  -1;转盘静止不动。
                  (/ dint cur-vw));按匀速运动计算时间。
              (* (+ (- cur-vw);有加速度情况下，按加速运动计算时间。
                    (sqrt (+ (* cur-vw cur-vw)
                             (* 2 cur-w dint))))
                 cur-w
                 1/4))))
      (define/private (set-dt)
        (set! cur-dt (cal-dt)))
      
      ;;计算耗费dt时间的转动角度dw(弧度):
      (define/public (cal-dw dt)
        (run dt)
        (+ (* cur-vw dt)
           (* 1/2 (cal-w) (* dt dt))))

      ;;运行dt时间后的状态：
      (define/private (run dt)
        (set-dr0);重置基准起始角cur-dr0。
        (for ([i nt])
          (send (vector-ref tracks i) run dt (cal-track-d i)));跟随圆盘运转轨道。
        (set-M);设置圆盘当前力矩。
        (set-w);设置圆盘当前角加速度。
        (set-vw dt));设置圆盘当前角速度。

      ;;计算并设置cur-dr0的值。
      (define/private (set-dr0);设置cur-dr0的值。
        (set! cur-dr0 (cal-dr0)))
      (define/private (cal-dr0);计算cur-dir0的值。
        (+ cur-dr0
           (if (< (get-vw) 0)
               (- d-int);圆盘转过d-int角度。
               (+ d-int))))

      ;;按步数自动旋转圆盘：
      ;用递归完成计算步数。
      (define/public (auto-run-step steps)
        (define (to-steps i);按设定步数完成自动运行。
          (set-dt);设定当前这一步所花费时间。
          (if (= cur-dt -1)
              (printf "时间为-1，可能是速度和加速度均为0。\n")
              (when (< i steps)
                (begin
                  (printf "\n\n第~a步:" i)
                  (printf "\n间隔时间：~a秒：" cur-dt)
                  (printf "\n当前起点角度：~a" cur-dr0))
                (run cur-dt)
                (to-steps (+ 1 i)))))
        (to-steps 0))

      ;;按时间自动旋转圆盘：
      (define/public (auto-run-time time time-gap)
        (for ([i (/ time time-gap)])
          (printf "\n\n时间点：~a" (* i time-gap))
          (run time-gap)))
      )))