#lang racket

(require racket/gui)

(provide main-frame
         statuebar/message)

;定义主界面=======================
(define main-frame
  (new frame%
       [label "永动机理论模型演示"]
       [width 800]
       [height 600]
       [border 5]))

;定义视图框架分割区域：========================
(define panel/all
  (new vertical-pane%
       [parent main-frame]
       [border 1]
       [alignment (list 'left 'top)]))

;定义工具栏区域：
(define panel/tool-bar
  (new horizontal-panel%
       [parent panel/all]
       ;[style (list 'border)]
       [alignment (list 'left 'center)]
       [min-height 30]
       [stretchable-height #f]))

;定义工作区域：
(define pane/work
  (new horizontal-pane%
       [parent panel/all]
       [alignment (list 'left 'top)]))

;定义绘图区域：
(define pane/draw
  (new vertical-pane%
       [parent pane/work]
       [alignment (list 'left 'top)]))

;定义画布区域：
(define pane/canvas
  (new horizontal-pane%
       [parent pane/draw]
       [vert-margin 2]
       [alignment (list 'left 'top)]))

;定义图表区域：
(define pane/chart
  (new horizontal-panel%
       [parent pane/draw]
       [vert-margin 2]
       [style (list 'border)]
       [alignment (list 'left 'bottom)]
       [min-height 100]
       [stretchable-height #f]))

;定义参数面板区域：
(define panel/parameter
  (new vertical-panel%
       [parent pane/work]
       ;[style (list 'border)]
       [alignment (list 'right 'top)]
       [min-width 100]
       [stretchable-width #f]))

;定义状态栏区域：
(define panel/statue-bar
  (new horizontal-panel%
       [parent panel/all]
       ;[style (list 'border)]
       [alignment (list 'left 'center)]
       [min-height 25]
       [stretchable-height #f]))

;定义视图：========================
;定义工具按钮宏：
(define-syntax-rule (button/tool-bar l c)
  (new button%
     [parent panel/tool-bar]
     [label l]
     [callback (lambda (item event)
                 c)]
     [style (list 'border)]))

;定义工具按钮：
(button/tool-bar "&R运行" (run))
(button/tool-bar "&S停止" (stop))
(button/tool-bar "&A关于" (about))
(button/tool-bar "&X退出" (exit-app))

;定义画布：-----------------------------
(define canvas
  (new canvas%
       [parent pane/canvas]
       [style (list 'border)]
       ;[label "永动机模型视图"]
       [min-width 100]
       [min-height 100]))

;定义参数面板控件：--------------------------------------------
;定义归类框宏：
(define-syntax-rule (rule/groupbox-parameter l)
  (new group-box-panel%
       [label l]
       [parent panel/parameter]
       [stretchable-height #f]
       [vert-margin 5]
       [horiz-margin 5]))
;定义归类面板宏：
(define-syntax-rule (rule/pane-parameter p)
  (new horizontal-panel%
       [parent p]
       [alignment (list 'left 'top)]
       [min-width 100]
       [stretchable-width #f]))
;定义信息框宏：
(define-syntax-rule (rule/pane/message-parameter p)
  (new vertical-pane%
       [parent p]
       [alignment (list 'right 'top)]
       [min-width 100]
       [stretchable-width #f]))
;定义文字框宏：
(define-syntax-rule (rule/pane/field-parameter p)
  (new vertical-pane%
       [parent p]
       [alignment (list 'left 'top)]
       [min-width 100]
       [stretchable-width #f]))

;定义面板布局：
(define groupbox/roll (rule/groupbox-parameter "轮子参数"))
(define pane/roll (rule/pane-parameter groupbox/roll))
(define pane/roll/message (rule/pane/message-parameter pane/roll))
(define pane/roll/field (rule/pane/field-parameter pane/roll))

(define groupbox/track (rule/groupbox-parameter "轨道参数"))
(define pane/track (rule/pane-parameter groupbox/track))
(define pane/track/message (rule/pane/message-parameter pane/track))
(define pane/track/field (rule/pane/field-parameter pane/track))

(define groupbox/ball (rule/groupbox-parameter "球参数"))
(define pane/ball (rule/pane-parameter groupbox/ball))
(define pane/ball/message (rule/pane/message-parameter pane/ball))
(define pane/ball/field (rule/pane/field-parameter pane/ball))

(define groupbox/run (rule/groupbox-parameter "运行参数"))
(define pane/run (rule/pane-parameter groupbox/run))
(define pane/run/message (rule/pane/message-parameter pane/run))
(define pane/run/field (rule/pane/field-parameter pane/run))
;定义提示宏：
(define-syntax-rule (rule/message-parameter l p)
  (new message%
       [label l]
       [parent p]
       [min-height 25]))
;定义字段宏：
(define-syntax-rule (rule/field-parameter p)
  (new text-field%
       [label #f]
       [parent p]
       [min-height 25]))

;定义参数控件:
(rule/message-parameter "外轮半径（米）：" pane/roll/message)
(define field/ro (rule/field-parameter pane/roll/field))

(rule/message-parameter "内轮半径（米）：" pane/roll/message)
(define field/ri (rule/field-parameter pane/roll/field))

(rule/message-parameter "轮子质量（千克）：" pane/roll/message)
(define field/roll-m (rule/field-parameter pane/roll/field))

(rule/message-parameter "轮子起始角（度）：" pane/roll/message)
(define field/a (rule/field-parameter pane/roll/field))

(rule/message-parameter "轨道数量（组）：" pane/track/message)
(define field/n (rule/field-parameter pane/track/field))

(rule/message-parameter "轨道质量（千克）：" pane/track/message)
(define field/track-m (rule/field-parameter pane/track/field))

(rule/message-parameter "球质量（千克）：" pane/ball/message)
(define field/ball-m (rule/field-parameter pane/ball/field))

(rule/message-parameter "旋转间隔（毫米）：" pane/run/message)
(define field/DC (rule/field-parameter pane/run/field))

(rule/message-parameter "时间间隔（秒）：" pane/run/message)
(define field/DT (rule/field-parameter pane/run/field))

;定义状态栏控件：------------------------------------------------------
(define statuebar/message
  (new message%
       [label "准备就绪……"]
       [parent panel/statue-bar]
       [min-height 25]))

;定义菜单=============================================================
(define menubar
  (new menu-bar%
       [parent main-frame]))

;定义程序菜单---------------------------------------------------------
(define menu/prog
  (new menu%
       [label "&P程序"]
       [parent menubar]))
(define menu/view
  (new menu%
       [label "&V视图"]
       [parent menubar]))
(define menu/help
  (new menu%
       [label "&H帮助"]
       [parent menubar]))

;创建子菜单的宏：
(define-syntax-rule (rule/menu-tiem p l c s h)
  (new menu-item%
       [parent p]
       [label l]
       [callback (lambda (item event) c)]
       [shortcut s]
       [help-string h]))

;创建子菜单：
(rule/menu-tiem menu/prog "&R运行" (run) #f "运行永动机模型")
(rule/menu-tiem menu/prog "&S停止" (stop) #f "运行永动机模型")
(rule/menu-tiem menu/prog "&X退出" (exit-app) #f "退出程序")

(rule/menu-tiem menu/view "&T显示工具条" (show-toolbar) #f "显示工具条")
(rule/menu-tiem menu/view "&P显示参数面板" (show-panel/parameter) #f "显示参数面板")
(rule/menu-tiem menu/view "&P显示状态条" (show-panel/statue-bar) #f "显示状态条")

(rule/menu-tiem menu/help "&A关于本程序" (about) #f "关于本程序")

;定义响应程序：====================================================================
(include "control-main.rkt")