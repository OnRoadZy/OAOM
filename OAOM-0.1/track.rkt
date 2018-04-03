;#lang racket
(module track racket
  (provide track%)

  (require "ball.rkt")

  (define track%
    (class object%
      (init [R0 100];外圆半径。
            [r0 20];内圆半径。
            [m0-ball 10];重力球质量。
            [m0 10];轨道质量。
            [d0 30]);轨道角度。

      (super-new)
      
      ;;轨道参数：
      (field [R R0];外圆半径。
             [r r0];内圆半径。
             [m-ball m0-ball];重力球质量。
             [m m0];轨道质量。
             [G (* m ga)];轨道重力。
             [St (cal-St)];轨道长度。
             [cur-d d0];轨道角度。
             [ball (init-ball)];重力球ball%对象。
             [M (cal-Mb)]);合力矩M。
      
      ;;计算轨道长度：
      (define/private (cal-St)
        (sqrt (- (* R R) (* r r))))

      ;;初始化重力球：
      (define/private (init-ball);初始化重力球。
        (new ball%
             [m0 m-ball]
             [d0 cur-d]
             [vd0 0]
             [St0 St]
             [S0 (init-S0)]))
      (define/private (init-S0);设置球在轨道上距近中心点的初始长度。
        (let ([d (adjust-d cur-d)])
          (if (and (>= d 0) (<= d 180))
              0
              St)))

      ;;计算重力球力产生的力矩：
      (define/private (cal-Mb);计算重力球力矩Mb。
        (* (cal-Fm (send ball get-G)) (cal-L (get-S))))
      ;;计算轨道产生的力矩：
      (define/private (cal-Mt);计算轨道力矩。
        (* (cal-Fm G) (cal-L (/ St 2))))
      ;;通用力臂L、分力Fm计算函数：
      (define/private (cal-L cur-S);计算重力球距圆盘圆心的距离(力臂)L。
          (sqrt (+ (* cur-S cur-S) (* r r))))
      (define/private (get-S);取得指定球的S值。
        (send ball get-S))
      (define/private (cal-Fm cur-G);计算分力Fm。
        (* cur-G
           (cos (degrees->radians (cal-dFm)))))
      (define/private (cal-dFm);计算力矩分力计算角度dFm。
        (- 180
           (+ (cal-dr) (cal-dS))))
      (define/private (cal-dr);计算dr值。
        (- cur-d 90))
      (define/private (cal-dS);计算球的当前位置与圆盘圆心的dS角。
        (radians->degrees (atan (get-S) r)))

      ;;计算合力矩：
      (define/private (cal-M)
        (+ (cal-Mb) (cal-Mt)))
      (define/private (set-M);设置合力矩M。
        (set! M (cal-M))
        #;(printf "\n轨道合力矩M:~a" (get-M)))
      (define/public (get-M);取得合力矩M值。
        M)

      ;;设置轨道角度d：
      (define/private (set-d d);设置d。
        (set! cur-d d))
      
      ;;运行dt时间后轨道的状态：
      (define/public (run dt d)
        (send ball run dt d);运行重力球ball。
        (set-d d);设置当前轨道角度d。
        (set-M));设置合力矩M。

      ;;计算轨道质量总和：
      (define/public (cal-m)
        (+ m m-ball))
  )))