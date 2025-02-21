#lang racket

(require racket/gui/base
         racket/class
         racket/list
         racket/path
         racket/system
         racket/port)

;; 定义主窗口
(define frame 
  (new frame% 
       [label "Novel Rocket 编辑器"]
       [width 1024]
       [height 768]))

;; 创建菜单栏
(define mb (new menu-bar% [parent frame]))
(define file-menu (new menu% [label "文件"] [parent mb]))
(define view-menu (new menu% [label "视图"] [parent mb]))

;; 创建水平分割面板
(define h-panel
  (new horizontal-panel%
       [parent frame]
       [spacing 1]
       [stretchable-width #t]
       [stretchable-height #t]))

;; 创建左侧面板（文件列表）
(define left-panel
  (new vertical-panel%
       [parent h-panel]
       [min-width 180]
       [stretchable-width #f]
       [stretchable-height #t]
       [style '(border)]))

;; 创建右侧主面板
(define main-panel
  (new vertical-panel%
       [parent h-panel]
       [stretchable-width #t]
       [stretchable-height #t]))

;; 创建编辑器面板
(define editor-panel
  (new vertical-panel%
       [parent main-panel]
       [stretchable-width #t]
       [stretchable-height #t]))

;; 创建标签页控件
(define tab-panel
  (new tab-panel%
       [parent editor-panel]
       [choices '("未命名")]
       [callback (lambda (tp e) 
                  (when (send tp get-selection)
                    (update-status)))]))

;; 创建终端面板
(define terminal-panel
  (new vertical-panel%
       [parent main-panel]
       [style '(border)]
       [min-height 150]
       [stretchable-height #f]))

;; 创建终端标题栏
(define terminal-title-panel
  (new horizontal-panel%
       [parent terminal-panel]
       [stretchable-height #f]))

(new message%
     [parent terminal-title-panel]
     [label " 终端"])

(define terminal-close-button
  (new button%
       [parent terminal-title-panel]
       [label "×"]
       [min-width 20]
       [callback (lambda (b e)
                  (send terminal-panel show #f))]))

;; 创建终端输出显示
(define terminal-text
  (new (class text%
         (super-new)
         (define/public (append-text str)
           (let ([len (send this last-position)])
             (send this insert str len)
             (send this scroll-to-position (send this last-position)))))))

(define terminal-canvas
  (new editor-canvas%
       [parent terminal-panel]
       [style '(no-hscroll auto-vscroll)]
       [min-height 120]
       [stretchable-width #t]))

(send terminal-canvas set-editor terminal-text)

;; 创建终端输入框
(define terminal-input
  (new text-field%
       [parent terminal-panel]
       [label ">"]
       [stretchable-width #t]
       [callback (lambda (t e)
                  (when (eq? (send e get-event-type) 'text-field-enter)
                    (let ([cmd (send t get-value)])
                      (send t set-value "")
                      (execute-command cmd))))]))

;; 终端命令执行函数
(define (execute-command cmd)
  (let* ([shell (if (eq? (system-type) 'windows) "cmd.exe" "/bin/bash")]
         [shell-arg (if (eq? (system-type) 'windows) "/c" "-c")]
         [process-output
          (with-output-to-string
            (lambda ()
              (system* shell shell-arg cmd)))])
    (send terminal-text append-text
          (format "> ~a\n~a" cmd process-output))))

;; 创建文件列表
(define file-list
  (new list-box%
       [parent left-panel]
       [label "文件"]
       [choices '()]
       [style '(single)]
       [stretchable-width #t]
       [stretchable-height #t]
       [callback (lambda (lb e)
                  (when (eq? (send e get-event-type) 'list-box-dclick)
                    (let* ([path (list-ref (get-directory-files) (send lb get-selection))])
                      (when (file-exists? path)
                        (create-tab path)))))]))

;; 文件路径列表
(define directory-files '())
(define (get-directory-files) directory-files)

;; 标签页数据结构
(struct tab-data (canvas text path) #:mutable)

;; 标签页列表
(define tabs '())

;; 创建新标签页
(define (create-tab [path #f])
  (let* ([canvas (new editor-canvas%
                      [parent tab-panel]
                      [style '(no-hscroll auto-vscroll)]
                      [stretchable-height #t])]
         [text (new (class text%
                     (super-new)
                     (define/augment (after-insert start len)
                       (update-status))
                     (define/augment (after-delete start len)
                       (update-status))))]
         [tab (tab-data canvas text path)])
    (send canvas set-editor text)
    (set! tabs (append tabs (list tab)))
    (when path
      (send text load-file path)
      (send tab-panel append (path->string (file-name-from-path path))))
    tab))

;; 获取当前标签页
(define (current-tab)
  (let ([idx (send tab-panel get-selection)])
    (list-ref tabs idx)))

;; 更新文件列表
(define (update-file-list [dir (current-directory)])
  (set! directory-files
        (filter (lambda (path)
                  (or (file-exists? path)
                      (directory-exists? path)))
                (map (lambda (name)
                       (build-path dir name))
                     (directory-list dir))))
  
  (send file-list clear)
  (for ([path directory-files])
    (send file-list append 
          (format "~a~a" 
                  (if (directory-exists? path) "📁 " "📄 ")
                  (path->string (file-name-from-path path))))))

;; 文件操作函数
(define (open-file)
  (let ([file (get-file)])
    (when file
      (create-tab file))))

(define (save-file)
  (let* ([tab (current-tab)]
         [text (tab-data-text tab)]
         [path (tab-data-path tab)]
         [file (or path (put-file))])
    (when file
      (send text save-file file)
      (set-tab-data-path! tab file)
      (let ([idx (send tab-panel get-selection)])
        (send tab-panel set-item-label 
              idx 
              (path->string (file-name-from-path file)))))))

(define (new-file)
  (create-tab)
  (send tab-panel append "未命名"))

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

;; 添加视图菜单项
(new menu-item%
     [label "显示终端"]
     [parent view-menu]
     [shortcut #\j]
     [callback (lambda (i e)
                (send terminal-panel show #t))])

;; 创建状态栏
(define status-bar
  (new message% 
       [parent main-panel]
       [label "字符数: 0"]
       [stretchable-width #t]))

;; 更新状态栏显示字符数
(define (update-status)
  (let* ([tab (current-tab)]
         [text (tab-data-text tab)])
    (send status-bar set-label
          (format "字符数: ~a" (send text last-position)))))

(module+ main
  ;; 创建初始标签页
  (create-tab)
  ;; 更新文件列表
  (update-file-list)
  ;; 显示窗口
  (send frame show #t)
  (update-status))

(module+ test
  (require rackunit)
  (check-true (is-a? frame frame%) "frame% 对象创建成功")
  (check-true (is-a? file-list list-box%) "list-box% 对象创建成功")
  (check-true (is-a? tab-panel tab-panel%) "tab-panel% 对象创建成功")) 