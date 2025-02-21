#lang racket

(require racket/gui/base
         racket/class)

;; 定义主窗口
(define frame 
  (new frame% 
       [label "Novel Rocket 编辑器"]
       [width 800]
       [height 600]))

;; 创建菜单栏
(define mb (new menu-bar% [parent frame]))
(define file-menu (new menu% [label "文件"] [parent mb]))

;; 创建主面板
(define main-panel
  (new vertical-panel% 
       [parent frame]
       [alignment '(left top)]))

;; 创建文本编辑区
(define editor-canvas
  (new editor-canvas% 
       [parent main-panel]
       [style '(no-hscroll auto-vscroll)]
       [stretchable-height #t]))

;; 创建文本编辑器
(define text
  (new (class text% 
         (super-new)
         (define/augment (after-insert start len)
           (update-status))
         (define/augment (after-delete start len)
           (update-status)))))

(send editor-canvas set-editor text)

;; 文件操作函数
(define (open-file)
  (let ([file (get-file)])
    (when file
      (send text load-file file)
      (send frame set-label (format "Novel Rocket 编辑器 - ~a" file)))))

(define (save-file)
  (let ([file (put-file)])
    (when file
      (send text save-file file)
      (send frame set-label (format "Novel Rocket 编辑器 - ~a" file)))))

(define (new-file)
  (send text erase)
  (send frame set-label "Novel Rocket 编辑器 - 未命名"))

;; 添加菜单项
(new menu-item% 
     [label "新建"]
     [parent file-menu]
     [callback (lambda (i e) (new-file))])

(new menu-item% 
     [label "打开"]
     [parent file-menu]
     [callback (lambda (i e) (open-file))])

(new menu-item% 
     [label "保存"]
     [parent file-menu]
     [callback (lambda (i e) (save-file))])

(new separator-menu-item% [parent file-menu])

(new menu-item%
     [label "退出"]
     [parent file-menu]
     [callback (lambda (i e) (send frame show #f))])

;; 创建状态栏
(define status-bar
  (new message% 
       [parent main-panel]
       [label "字符数: 0"]
       [stretchable-width #t]))

;; 更新状态栏显示字符数
(define (update-status)
  (send status-bar set-label
        (format "字符数: ~a" (send text last-position))))

(module+ main
  (send frame show #t)
  (update-status))

(module+ test
  (require rackunit)
  (check-true (is-a? text text%) "text% 对象创建成功")
  (check-true (is-a? frame frame%) "frame% 对象创建成功")) 