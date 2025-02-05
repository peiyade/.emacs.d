(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; 禁用启动画面
(setq inhibit-startup-screen t)
;; 禁用菜单栏
(menu-bar-mode -1)
;; 禁用工具栏
(tool-bar-mode -1)
;; 禁用滚动条
(scroll-bar-mode -1)
;; 全局启用行号显示
(global-display-line-numbers-mode 1)
;; 使用可视提示代替声音提示
(setq visible-bell t)

;; 设置语言环境为UTF-8
(set-language-environment "UTF-8")
;; 设置默认文件编码为UTF-8
(setq default-buffer-file-coding-system 'utf-8)

;; 配置国内ELPA镜像源，加速包下载
(setq package-archives
      '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
        ("org"   . "http://mirrors.cloud.tencent.com/elpa/org/")))

;; 加载package.el包管理系统
(require 'package)
;; 如果包列表为空，则刷新包列表
(unless package-archive-contents
  (package-refresh-contents))

;; 自动安装use-package包管理工具
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; 设置use-package自动确保包已安装
(setq use-package-always-ensure t)

;; 设置默认字体为Monaco，字号14
(set-face-attribute 'default nil :font "Monaco-14")
;; 设置中文字体为PingFang SC
(set-fontset-font t 'han (font-spec :name "PingFang SC"))

;; 使用doom-themes主题包
(use-package doom-themes
  :config
  ;; 加载doom-one主题
  (load-theme 'doom-one t)
  ;; 配置doom主题的org-mode样式
  (doom-themes-org-config))

;; 配置org-roam知识管理系统
(use-package org-roam
  :init
  ;; 确认使用org-roam v2版本
  (setq org-roam-v2-ack t)
  :custom
  ;; 设置org-roam笔记存储目录
  (org-roam-directory "~/Documents/org-roam")
  ;; 设置org-roam数据库位置
  (org-roam-db-location "~/Documents/org-roam/org-roam.db")
  ;; 配置笔记模板
  (org-roam-capture-templates
   '(("p" "Paper Note" plain
      "%?"
      :if-new (file+head "papers/${slug}.org"
                         "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+TAGS: paper\n\n")
      :unnarrowed t)
     ("m" "Math Object" plain
      "%?"
      :if-new (file+head "math/${slug}.org"
                         "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+TAGS: math\n\n* Definition\n* Properties\n* Applications\n* References\n")
      :unnarrowed t)))
  :config
  ;; 启用数据库自动同步模式
  (org-roam-db-autosync-mode)
  ;; 设置节点显示模板
  (setq org-roam-node-display-template
        "${title:*} ${tags:10}")
  ;; 启用全局补全
  (setq org-roam-completion-everywhere t)
  ;; 配置引用模板
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :if-new (file+head "references/${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+TAGS: reference\n\n")
           :unnarrowed t)))
  ;; 设置标签来源
  (setq org-roam-tag-sources '(prop last-directory))
  ;; 排除archive目录下的org文件
  (setq org-roam-file-exclude-regexp ".*/archive/.*\\.org$"))

;; 配置AUCTeX LaTeX环境
(use-package tex
  :ensure auctex
  :config
  ;; 设置使用XeTeX引擎
  (setq TeX-engine 'xetex)
  ;; 保存时不询问
  (setq TeX-save-query nil)
  ;; 显示编译过程
  (setq TeX-show-compilation t)
  ;; 启用PDF模式
  (setq TeX-PDF-mode t)
  ;; 启用源代码关联模式
  (setq TeX-source-correlate-mode t)
  ;; LaTeX模式钩子
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  ;; 集成reftex
  (setq reftex-plug-into-AUCTeX t))

;; 配置org-mode
(use-package org
  :config
  ;; 启用缩进模式
  (setq org-startup-indented t)
  ;; 配置导出时上下标处理
  (setq org-export-with-sub-superscripts '{})
  ;; 启用源代码语法高亮
  (setq org-src-fontify-natively t)
  ;; 设置LaTeX PDF编译命令
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))
  ;; 设置默认LaTeX文档类
  (setq org-latex-default-class "ctexart")
  ;; 配置LaTeX公式显示选项
  (setq org-format-latex-options '(:foreground default :background default :scale 2.0)))

;; 配置pdf-tools
(use-package pdf-tools
  :config
  ;; 安装pdf-tools
  (pdf-loader-install)
  ;; 设置PDF缩放因子
  (setq pdf-view-resize-factor 1.1)
  ;; PDF查看模式禁用行号
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

;; 配置company-mode自动补全
(use-package company
  :config
  ;; 全局启用company-mode
  (global-company-mode)
  ;; 设置最小补全前缀长度
  (setq company-minimum-prefix-length 1)
  ;; 设置补全延迟时间
  (setq company-idle-delay 0.1))

;; macOS特定配置
(when (eq system-type 'darwin)
  ;; 设置Option键为Meta键
  (setq mac-option-modifier 'meta)
  ;; 设置Command键为Super键
  (setq mac-command-modifier 'super)
  ;; 设置默认输入法
  (setq default-input-method "MacOSX")
  ;; 禁用代理图标
  (setq ns-use-proxy-icon nil)
  ;; 设置窗口标题格式
  (setq frame-title-format "%b"))

;; 全局快捷键绑定
(global-set-key (kbd "C-c l") 'org-store-link)  ;; 存储org链接
(global-set-key (kbd "C-c a") 'org-agenda)      ;; 打开org议程
(global-set-key (kbd "C-c c") 'org-capture)     ;; 快速捕获笔记
(global-set-key (kbd "C-c b") 'org-switchb)     ;; 切换org缓冲区

;; 性能优化配置
;; 设置垃圾回收阈值为50MB
(setq gc-cons-threshold (* 50 1000 1000))
;; 启动后设置垃圾回收阈值为2MB
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))
