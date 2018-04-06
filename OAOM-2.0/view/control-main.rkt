;control-main.rkt
;定义main-frame的响应程序：==============================
;用include载入frame-main.rkt使用。

(require racket/draw
         "../model/roll.rkt"
         "../model/track.rkt"
         "../model/ball.rkt"
         "../model/value.rkt")

(provide init-view)

;定义全局oaom-roll对象：
(define oaom-roll (new roll%))

;初始化系统时钟：
(define oaom-timer
  (new timer%
       [interval 10]
       [notify-callback
        (lambda ()
          (update-draw-oaom))]))

;定义运行时间（初始为10分钟）：
(define run-times 10)

;视图模型参数初始化：
(define (init-view)
  (let ([roll oaom-roll])
    ;设置轮子参数：
    (send field/ro set-value (number->string (get-field ro roll)))
    (send field/ri set-value (number->string (get-field ri roll)))
    (send field/roll-m set-value (number->string (get-field m roll)))
    (send field/a set-value (number->string (get-field a roll)))
    (send field/n set-value (number->string (get-field n roll)))
    ;设置轨道和球参数：
    (let* ([tb (vector-ref (get-field track/balls roll) 0)]
           [track (track/ball-struct-track tb)]
           [ball (track/ball-struct-ball tb)])
      (begin
        (send field/track-m set-value (number->string (get-field m track)))
        (send field/ball-m set-value (number->string (get-field m ball)))))))

;定义运行程序：
(define (run)
  (display "Ok!Run is ready.Run程序已经准备好。\n")
  ;设置运行参数：
  (set-oaom-init #:ro (string->number (send field/ro get-value))
                 #:ri (string->number (send field/ri get-value))
                 #:a (string->number (send field/a get-value))
                 #:n (string->number (send field/n get-value))
                 #:roll-m (string->number (send field/roll-m get-value))
                 #:track-m (string->number (send field/track-m get-value))
                 #:ball-m (string->number (send field/ball-m get-value)))

  ;重置模型数据：
  (send oaom-roll reset-oaom)
  ;在画布上绘制模型：
  (draw-oaom oaom-roll)
  ;设定时钟：
  (start-oaom-timer))

;定义停止程序：
(define (stop)
  (send oaom-timer stop))
   
;设置时钟的宏：
(define-syntax-rule (start-oaom-timer)
  (send oaom-timer start
        (exact-truncate (* (get-field dt oaom-roll) 1000))))
  
;更新oaom的绘制：
(define (update-draw-oaom)
  (send oaom-roll update-oaom)
  (draw-oaom oaom-roll)
  ;重置时钟间隔：
  (start-oaom-timer))
  
;在画布上绘制模型：
(define (draw-oaom roll)
  (let ([dc (send canvas get-dc)])
    (begin
      (define-values (w h) (send canvas get-virtual-size))
      (send dc clear)
      (send roll draw dc w h))))
 
;退出程序：
(define (exit-app)
  (send main-frame on-exit))

;显示工具条：
(define (show-toolbar) (show-view/object panel/tool-bar))
;显示参数面板：
(define (show-panel/parameter) (show-view/object panel/parameter))
;显示状态栏：
(define (show-panel/statue-bar) (show-view/object panel/statue-bar))

;定义显示构建宏：
(define-syntax-rule (show-view/object object)
  (if (send object is-shown?)
      (send object show #f)
      (send object show #t)))

;显示关于对话框：
(define (about)
  (message-box "关于永动机模型程序"
               "永动机模型程序：是一个模拟永动机的数学模型程序，用Racket编写。\n
本程序模拟在一个给定参数下的永动机模型的运行情况，显示或获得其运行状态参数。\n该程序展示了Racket语言的类与对象、宏、GUI编程、绘图编程等。\n
作者：ZHY"  
               main-frame
               '(ok caution)))