;#lang racket

(module ball racket
  (provide ga uk adjust-d ball%)
  
  ;;以下数据做调试时临时使用，实际使用时采用统一的全局设置。
  (define ga 9.80665) ;gravitational acceleration重力加速度。
  (define uk 0.15) ;friction coefficient动摩擦系数。
  (define (adjust-d deg);将角度调整为360度以内。
      (if (>= deg 360)
        (adjust-d (- deg 360))
        deg))
  
  (define ball%
    (class object%
      ;初始化球的参数：
      (init m0;mass球的质量。
            d0;轨道初始角度(单位：角度)。
            [vd0 0];球沿轨道方向初始速度(>0，向中心运动；<0；向外围运动)。
            [St0 100];轨道长度。
            [S0 0]);球距轨道近中心端点初始长度。
      
      (field [m m0]
             [G (* m ga)];gravity重力值G。
             [cur-d d0];direction当前轨道角度(单位：角度)。
             [cur-vd vd0];沿轨道速度vd(>0，向中心运动；<0；向外围运动)。
             [St St0];track轨道长度。
             [cur-S S0];球距轨道近中心端点长度。
             [cur-N (cal-N)]);球对轨道的垂直压力。
      
      (super-new)

      ;取得重力值：
      (define/public (get-G)
        G)
      
      ;压力(pressure)N：
      (define/private (cal-N);计算N值。
        (* G (cos (degrees->radians cur-d))))
      (define/private (set-N);设置N值。
        (set! cur-N (cal-N)))
      (define/private (get-N);取得N值。
        cur-N);
      
      ;轨道方向动力F：
      (define/private (cal-F)
        (* G (sin (degrees->radians cur-d))))
      
      ;摩擦力(friction)Fk：
      (define/private (cal-Fk);计算摩擦力。
        (* (get-N) uk))
      
      ;球沿轨道方向加速度ad：
      (define/private (cal-Fd);求沿轨道方向的合力。
        #;(printf "cur-d:~a\n" cur-d)
        (let ([d (adjust-d cur-d)])
          (cond
            [(and (> d 0) (< d 180));此时F>0，球向中心运动。
             (- (cal-F) (cal-Fk))]
            [(and (> d 180) (< d 360));此时F<0，球向外围运动。
             (+ (cal-F) (cal-Fk))]
            [(= d 180);此时仅剩摩擦力，球向中心运动。
             (cal-Fk)]
            [(= d 0);此时仅剩摩擦力，球向外围运动。
             (* -1 (cal-Fk))])))
      (define/private (cal-ad);计算球的加速度(>0，向中心运动；<0，向外围运动)。
        (/ (cal-Fd) m))
      
      ;计算加速度作用dt时间后的沿轨道速度vd:
      (define/private (cal-vd dt);计算vd。
        (+ cur-vd (* (cal-ad) dt)))
      (define/private (set-vd dt);设置vd。
        (set! cur-vd (cal-vd dt)))
      
      ;设置轨道角度：
      (define/private (set-d d);设置d。
        (set! cur-d d))
      
      ;计算dt时间内球沿轨道运动后的路程S：
      (define/private (cal-Sdt dt);dt时间运行路程(>0，向中心运动；<0，向外围运动)。
        (+ (* cur-vd dt)
           (* 1/2 (cal-ad) (* dt dt))))
      (define/private (set-S dt);设置S的值。
        (let ([S (- cur-S (cal-Sdt dt))])
          #;(begin;做运动分析、力学分析、运动力学分析:
            (printf "cur-S:~a,Sdt:~a,S:~a\n" cur-S (cal-Sdt dt) S)
            (printf "N:~a,F:~a,Fk:~a,Fd:~a\n" (get-N) (cal-F) (cal-Fk) (cal-Fd))
            (printf "ad:~a,cur-vd:~a\n" (cal-ad) (cal-vd dt)))
          (cond [(<= S 0)
                 (begin
                   (set! cur-S 0)
                   (set! cur-vd 0))]
                [(>= S St)
                 (begin
                   (set! cur-S St)
                   (set! cur-vd 0))]
                [else (set! cur-S S)])))
      (define/public (get-S);取得S的值。
        cur-S)
      
      ;运行dt时间后球的状态(注意计算顺序)：
      ;d => 角度
      (define/public (run dt d)
        ;设置dt后的S值（用dt前的参数计算）：
        (set-S dt)
        ;设置dt后相关参数变化：
        (set-d d)
        (set-N)
        (set-vd dt)))))
