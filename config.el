;; 关闭启动画面
(setq inhibit-startup-message t)

;; 关闭工具栏、菜单栏、滚动条
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; 设置默认编码
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 备份文件设置
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; 添加 load-path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; 加载 use-package (假设已通过 git submodule 添加)
(require 'use-package)

;; 不需要确保包已安装，因为我们使用 git submodule
(setq use-package-always-ensure nil)

(add-to-list 'load-path "~/.emacs.d/site-lisp/use-package")
(require 'use-package)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/use-package/"))

(use-package org
  :load-path "~/.emacs.d/site-lisp/org-mode/lisp/"
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t))

(require 'org-id)

;; 设置 Org Babel 支持的语言
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

;; 自动生成 config.el
(defun tangle-config-org ()
  "Tangle config.org on save."
  (when (equal (buffer-file-name) 
               (expand-file-name "~/.emacs.d/config.org"))
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-config-org)

(use-package pdf-occur
    :commands (pdf-occur-global-minor-mode))
  (use-package pdf-history
    :commands (pdf-history-minor-mode))
  (use-package pdf-links
    :commands (pdf-links-minor-mode))
  (use-package pdf-outline
    :commands (pdf-outline-minor-mode))
  (use-package pdf-annot
    :commands (pdf-annot-minor-mode))
  (use-package pdf-sync
    :commands (pdf-sync-minor-mode))

(use-package tablist
  :load-path "~/.emacs.d/site-lisp/tablist")

(use-package pdf-tools
  :load-path "~/.emacs.d/site-lisp/pdf-tools/lisp/"
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; 初始化 pdf-tools
  (pdf-tools-install)
  
  ;; 使用 pdf-tools 打开 PDF 文件
  (setq-default pdf-view-display-size 'fit-page)
  
  ;; 禁用 pdf-view-mode 中的行号显示
  (add-hook 'pdf-view-mode-hook (lambda () 
                                  (display-line-numbers-mode -1)))
  
  ;; 启用 pdf-annot-minor-mode 以支持注释功能
  (add-hook 'pdf-view-mode-hook 'pdf-annot-minor-mode)
  
  ;; 启用 pdf-outline-minor-mode 以支持大纲功能
  (add-hook 'pdf-view-mode-hook 'pdf-outline-minor-mode)
  
  ;; 启用 pdf-sync-minor-mode 以支持同步功能
  (add-hook 'pdf-view-mode-hook 'pdf-sync-minor-mode))

;; 添加 PDF 到 Org Babel 支持的语言中
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (latex . t))))

;; 设置 Org 导出 PDF 时使用 pdf-tools 打开
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . default)
        ("\\.pdf\\'" . "emacs %s")))
