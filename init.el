;; -*- lexical-binding: t; -*-
;; 基础设置
(setq inhibit-startup-screen t)    ; 禁用启动画面
(menu-bar-mode -1)                 ; 禁用菜单栏
(tool-bar-mode -1)                 ; 禁用工具栏
(scroll-bar-mode -1)               ; 禁用滚动条
(global-display-line-numbers-mode 1) ; 显示行号
(setq visible-bell t)              ; 用视觉提示代替声音提示

;; 设置中文环境
(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)

;; 国内软件源配置
;; 方案1 - 中科大源（推荐）
(setq package-archives
      '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
	("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
        ("org"   . "http://mirrors.cloud.tencent.com/elpa/org/")))

;; 包管理系统初始化
(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; 自动安装 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; 字体设置（根据实际情况修改路径）
(set-face-attribute 'default nil :font "Monaco-14")
(set-fontset-font t 'han (font-spec :name "PingFang SC"))

;; LaTeX 配置
(use-package tex
  :ensure auctex
  :config
  (setq TeX-engine 'xetex)        ; 使用 XeLaTeX
  (setq TeX-save-query nil)       ; 保存时自动编译
  (setq TeX-show-compilation t)   ; 显示编译窗口
  (setq TeX-PDF-mode t)           ; 默认生成 PDF
  (setq TeX-source-correlate-mode t) ; 正向搜索
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

;; Org-mode 配置
(use-package org
  :config
  (setq org-startup-indented t)       ; 启用缩进
  (setq org-export-with-sub-superscripts '{}) ; 禁用特殊符号转换
  (setq org-src-fontify-natively t)   ; 代码块语法高亮
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f")) ; 编译两次解决目录等问题
  (setq org-latex-default-class "ctexart") ; 使用中文文档类
  (setq org-format-latex-options '(:foreground default :background default :scale 2.0)))

;; PDF 工具配置（用于反向搜索）
(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq pdf-view-resize-factor 1.1)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

;; 自动补全配置
(use-package company
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1))

;; 主题设置
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; 性能优化
(setq gc-cons-threshold (* 50 1000 1000)) ; 提高GC阈值
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; Mac 特定设置
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)     ; 设置 Option 键为 Meta
  (setq mac-command-modifier 'super)   ; 设置 Command 键为 Super
  (setq default-input-method "MacOSX") ; 启用系统输入法
  (setq ns-use-proxy-icon nil)         ; 禁用标题栏图标
  (setq frame-title-format "%b"))      ; 窗口标题显示缓冲区名称

;; 自定义快捷键
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(auctex company doom-themes pdf-tools)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
