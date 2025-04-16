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

;; 现代化补全框架
(use-package vertico
  :load-path "~/.emacs.d/site-lisp/vertico"
  :init
  (require 'vertico)
  (vertico-mode))

(use-package consult
  :load-path "~/.emacs.d/site-lisp/consult"
  :bind (("C-s" . consult-line)
         ("C-r". consult-history)
         ("M-y". consult-yank-pop)
         ("C-x b". consult-buffer)
         ("C-c f" . consult-find)))

(use-package orderless
  :load-path "~/.emacs.d/site-lisp/orderless"
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 使用 Emacs 内置的 org-mode
(use-package org
  :ensure nil  ; 不需要安装，使用内置的
  :config
  ;; 美化设置
  (setq org-ellipsis " ▾ ")
  (setq org-hide-emphasis-markers t)
  ;; 标题美化
  (setq org-startup-indented t)
  (setq org-indent-mode-turns-on-hiding-stars t)
  ;; 设置不同级别标题的大小
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  
  ;; 设置标题缩进
  (setq org-indent-indentation-per-level 2))

;; 单独配置 org-id，确保在 org 加载后
(use-package org-id
  :after org
  :ensure nil
  :config
  (setq org-id-method 'ts)  ; 使用时间戳方法
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations"))

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

;; Org-Roam 配置
(use-package org-roam
  :load-path "~/.emacs.d/site-lisp/org-roam"
  :custom
  (org-roam-directory (file-truename "~/org-roam"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "默认" plain
      "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "数学概念" plain
      "* 定义\n%?\n\n* 性质\n\n* 例子\n\n* 相关概念\n\n* 参考文献\n"
      :target (file+head "math/${slug}.org"
                        "#+title: ${title}\n#+filetags: :math:concept:\n")
      :unnarrowed t)
     ("t" "定理" plain
      "* 陈述\n%?\n\n* 证明\n\n* 推论\n\n* 应用\n\n* 参考文献\n"
      :target (file+head "math/theorems/${slug}.org"
                        "#+title: ${title}\n#+filetags: :math:theorem:\n")
      :unnarrowed t)
     ("p" "PDE问题" plain
      "* 问题描述\n%?\n\n* 边界条件\n\n* 解法思路\n\n* 解的性质\n\n* 相关文献\n"
      :target (file+head "math/pde/${slug}.org"
                        "#+title: ${title}\n#+filetags: :math:pde:\n")
      :unnarrowed t)
     ("r" "研究笔记" plain
      "* 研究问题\n%?\n\n* 相关工作\n\n* 方法\n\n* 结果\n\n* 下一步计划\n"
      :target (file+head "research/${slug}.org"
                        "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+filetags: :research:\n")
      :unnarrowed t)))
  :bind ((:map global-map
          ("C-c n f" . org-roam-node-find)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n c" . org-roam-capture)
          ("C-c n l" . org-roam-buffer-toggle))
         (:map org-mode-map
          ("C-M-i" . completion-at-point)))
  :config
  ;; 配置org-roam-ui
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t)
  
  ;; 创建必要的目录
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))
  (unless (file-exists-p (expand-file-name "math" org-roam-directory))
    (make-directory (expand-file-name "math" org-roam-directory) t))
  (unless (file-exists-p (expand-file-name "math/theorems" org-roam-directory))
    (make-directory (expand-file-name "math/theorems" org-roam-directory) t))
  (unless (file-exists-p (expand-file-name "math/pde" org-roam-directory))
    (make-directory (expand-file-name "math/pde" org-roam-directory) t))
  (unless (file-exists-p (expand-file-name "research" org-roam-directory))
    (make-directory (expand-file-name "research" org-roam-directory) t))
  
  ;; 启动org-roam
  (org-roam-db-autosync-mode))

;; Org-Roam-UI配置
(use-package org-roam-ui
  :load-path "~/.emacs.d/site-lisp/org-roam-ui"
  :after org-roam
  :config
  (setq org-roam-ui-browser-function #'browse-url-default-browser))

;; LaTeX 和数学公式输入增强
(use-package cdlatex
  :load-path "~/.emacs.d/site-lisp/cdlatex"
  :hook (org-mode . org-cdlatex-mode)
  :config
  ;; 自定义数学符号输入
  (setq cdlatex-math-symbol-alist
        '(("p" "\\partial" "\\partial")
          ("e" "\\varepsilon" "\\epsilon")
          ("d" "\\delta" "\\Delta")
          ("l" "\\lambda" "\\Lambda")
          ("g" "\\gamma" "\\Gamma")
          ("o" "\\omega" "\\Omega")
          ("u" "\\nabla" nil)
          ("s" "\\sigma" "\\Sigma")
          ("i" "\\int\\limits_{-\\infty}^{\\infty}" nil)
          ("8" "\\infty" nil)
          ("I" "\\oint" nil)
          ("*" "\\times" nil)
          ("." "\\cdot" nil)
          ("<" "\\langle" nil)
          (">" "\\rangle" nil)
          ("~" "\\tilde" nil)
          ("^" "\\hat" nil)
          ("/" "\\frac{?}{}"))
        cdlatex-math-modify-alist
        '(("b" "\\mathbf" nil t nil nil)
          ("c" "\\mathcal" nil t nil nil)
          ("B" "\\boldsymbol" nil t nil nil)
          ("r" "\\mathrm" nil t nil nil)
          ("v" "\\vec" nil t nil nil)))
  
  ;; 自定义环境模板
  (setq cdlatex-env-alist
        '(("eqn" "\\begin{equation}
?\\end{equation}"
           nil)
          ("ali" "\\begin{align}
?\\end{align}"
           nil)
          ("gat" "\\begin{gather}
?\\end{gather}"
           nil)
          ("thm" "\\begin{theorem}
?\\end{theorem}"
           nil)
          ("lem" "\\begin{lemma}
?\\end{lemma}"
           nil)
          ("prf" "\\begin{proof}
?\\end{proof}"
           nil)
          ("def" "\\begin{definition}
?\\end{definition}"
           nil)
          ("prop" "\\begin{proposition}
?\\end{proposition}"
           nil))))

;; 配置org-ref用于文献管理

(use-package compat
  :load-path "~/.emacs.d/site-lisp/compat")

(use-package dash
  :load-path "~/.emacs.d/site-lisp/dash.el")

(use-package websocket
  :load-path "~/.emacs.d/site-lisp/emacs-websocket")

(use-package f
  :load-path "~/.emacs.d/site-lisp/f.el")

(use-package s
  :load-path "~/.emacs.d/site-lisp/s.el")

(use-package parsebib
  :load-path "~/.emacs.d/site-lisp/parsebib")

(use-package async
  :load-path "~/.emacs.d/site-lisp/emacs-async")

(use-package biblio
  :load-path "~/.emacs.d/site-lisp/biblio.el")

(use-package ox-pandoc
  :load-path "~/.emacs.d/site-lisp/ox-pandoc")

(use-package ht
  :load-path "~/.emacs.d/site-lisp/ht.el")

(use-package citeproc
  :load-path "~/.emacs.d/site-lisp/citeproc-el")

(use-package queue
  :load-path "~/.emacs.d/site-lisp/queue")

(use-package avy
  :load-path "~/.emacs.d/site-lisp/avy")

(use-package request
  :load-path "~/.emacs.d/site-lisp/emacs-request")

(use-package helm
  :load-path "~/.emacs.d/site-lisp/helm")

(use-package helm-bibtex
  :load-path "~/.emacs.d/site-lisp/helm-bibtex"
  :after (helm))



(use-package org-ref
  :load-path "~/.emacs.d/site-lisp/org-ref"
  :after (org dash f s parsebib helm-bibtex)
  :config
  (setq org-ref-default-bibliography '("~/org-roam/bibliography/references.bib")
        org-ref-pdf-directory "~/org-roam/bibliography/pdfs/"
        org-ref-notes-directory "~/org-roam/bibliography/notes/")
  
  ;; 创建必要的目录
  (unless (file-exists-p "~/org-roam/bibliography")
    (make-directory "~/org-roam/bibliography" t))
  (unless (file-exists-p org-ref-pdf-directory)
    (make-directory org-ref-pdf-directory t))
  (unless (file-exists-p org-ref-notes-directory)
    (make-directory org-ref-notes-directory t))
  
  ;; 设置引用格式
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-export-latex-format-toc-function 'org-export-latex-no-toc
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
        org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :AUTHOR: %a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"))

;; 配置org-ai与大模型集成
(use-package org-ai
  :load-path "~/.emacs.d/site-lisp/org-ai"
  :after (org websocket)
  :custom
  ;; DeepSeek配置
  (org-ai-default-chat-model "deepseek")
  (org-ai-deepseek-api-key (getenv "DEEPSEEK_API_KEY"))
  (org-ai-deepseek-api-base-url "https://api.deepseek.com/v1")
  
  ;; 其他模型配置
  (org-ai-openai-api-key (getenv "OPENAI_API_KEY"))
  (org-ai-anthropic-api-key (getenv "ANTHROPIC_API_KEY"))
  
  ;; 提示模板
  (org-ai-prompt-templates
   '(("math-explain" . "请详细解释以下数学概念或定理：\n\n$x")
     ("math-proof" . "请提供以下定理的详细证明：\n\n$x")
     ("math-example" . "请提供一个关于$x的具体例子，并详细解释")
     ("pde-solve" . "请解决以下偏微分方程问题并详细说明解法步骤：\n\n$x")
     ("latex-fix" . "请修正以下LaTeX代码中的错误：\n\n$x")
     ("summarize-paper" . "请总结以下研究论文的主要内容、方法和贡献：\n\n$x")))
  
  :config
  ;; 启用org-ai功能
  (org-ai-global-mode)
  
  ;; 自定义函数：快速插入数学解释
  (defun my/org-ai-math-explain ()
    "使用AI解释选中的数学内容"
    (interactive)
    (if (use-region-p)
        (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (insert (format "\n** AI解释\n#+begin_ai\n请详细解释以下数学概念或定理：\n\n%s\n#+end_ai\n" content)))
      (message "请先选择要解释的数学内容")))
  
  ;; 自定义函数：快速生成证明
  (defun my/org-ai-math-proof ()
    "使用AI生成选中定理的证明"
    (interactive)
    (if (use-region-p)
        (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (insert (format "\n** AI证明\n#+begin_ai\n请提供以下定理的详细证明：\n\n%s\n#+end_ai\n" content)))
      (message "请先选择要证明的定理")))
  
  ;; 自定义函数：PDE求解辅助
  (defun my/org-ai-pde-solve ()
    "使用AI辅助解决PDE问题"
    (interactive)
    (if (use-region-p)
        (let ((content (buffer-substring-no-properties (region-beginning) (region-end))))
          (deactivate-mark)
          (insert (format "\n** AI求解\n#+begin_ai\n请解决以下偏微分方程问题并详细说明解法步骤：\n\n%s\n#+end_ai\n" content)))
      (message "请先选择要求解的PDE问题")))
  
  ;; 绑定快捷键
  (global-set-key (kbd "C-c a e") 'my/org-ai-math-explain)
  (global-set-key (kbd "C-c a p") 'my/org-ai-math-proof)
  (global-set-key (kbd "C-c a s") 'my/org-ai-pde-solve)
  (global-set-key (kbd "C-c a i") 'org-ai-prompt)
  (global-set-key (kbd "C-c a c") 'org-ai-chat))

;; 数学研究相关快捷键优化
(use-package hydra
  :load-path "~/.emacs.d/site-lisp/hydra"
  :config
  ;; 创建数学笔记相关操作的hydra菜单
  (defhydra hydra-math-notes (:color blue :hint nil)
    "
^笔记操作^          ^公式^              ^引用^           ^AI辅助^
^^^^^^^^-----------------------------------------------------------------
_f_: 查找笔记      _e_: 编辑公式      _c_: 插入引用    _E_: AI解释
_i_: 插入链接      _n_: 新公式块      _b_: 打开文献    _P_: AI证明
_t_: 添加标签      _a_: 对齐环境      _r_: 刷新文献    _S_: AI求解PDE
_d_: 日常笔记      _s_: 插入符号      _p_: 预览PDF     _C_: AI对话
"
    ("f" org-roam-node-find)
    ("i" org-roam-node-insert)
    ("t" org-roam-tag-add)
    ("d" (org-roam-capture- :node (org-roam-node-create) :templates '(("d" "默认" plain "%?" :target (file+head "${slug}.org" "#+title: ${title}\n") :unnarrowed t))))
    ("e" org-cdlatex-environment)
    ("n" (lambda () (interactive) (insert "\\begin{equation}\n\n\\end{equation}") (forward-line -1)))
    ("a" (lambda () (interactive) (insert "\\begin{align}\n\n\\end{align}") (forward-line -1)))
    ("s" cdlatex-math-symbol)
    ("c" org-ref-cite-insert-ivy)
    ("b" org-ref-open-bibtex-notes)
    ("r" org-ref-bibliography-refresh)
    ("p" org-latex-preview)
    ("E" my/org-ai-math-explain)
    ("P" my/org-ai-math-proof)
    ("S" my/org-ai-pde-solve)
    ("C" org-ai-chat)
    ("q" nil "退出" :color blue))
  
  ;; 绑定全局快捷键
  (global-set-key (kbd "C-c m") 'hydra-math-notes/body))

;; 配置org-capture模板
(with-eval-after-load 'org
  (setq org-capture-templates
        '(("t" "待办事项" entry
           (file+headline "~/org-roam/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "研究笔记" entry
           (file+headline "~/org-roam/research_notes.org" "Notes")
           "* %?\n  %U\n  %i")
          ("j" "研究日志" entry
           (file+datetree "~/org-roam/research_journal.org")
           "* %?\n  %U\n  %i")
          ("i" "研究想法" entry
           (file+headline "~/org-roam/research_ideas.org" "Ideas")
           "* %?\n  %U\n  %i"))))

;; Meow 模态编辑配置
(use-package meow
  :load-path "~/.emacs.d/site-lisp/meow"
  :init
  ;; 加载布局定义文件
  (require 'meow-cheatsheet-layout)
  ;; 加载meow核心模块
  (require 'meow)
  
  ;; 为 macOS 优化的 Meow 设置
  (defun meow-setup-mac ()
    ;; 设置 QWERTY 布局
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    
    ;; 移动模式键位
    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    
    ;; Leader 键设置 (使用空格键)
    (meow-leader-define-key
     ;; 数字参数
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     
     ;; 常用命令
     '("b" . consult-buffer)
     '("f" . find-file)
     '("w" . save-buffer)
     '("k" . kill-this-buffer)
     '("s" . save-some-buffers)
     '("d" . dired-jump)
     '("o" . delete-other-windows)
     '("v" . split-window-vertically)
     '("h" . split-window-horizontally)
     '("p" . project-find-file)
     '("g" . magit-status)
     
     ;; 帮助
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    
    ;; 普通模式键位
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  
  ;; 调用设置函数
  (meow-setup-mac)
  
  ;; Mac 特定优化
  (setq mac-command-modifier 'meta)       ;; 将 Command 键映射为 Meta
  (setq mac-option-modifier 'super)       ;; 将 Option 键映射为 Super
  (setq mac-right-option-modifier 'none)  ;; 右 Option 键保持原样，用于输入特殊字符
  
  ;; 启用 meow 全局模式
  :config
  ;; 设置模式指示器
  (setq meow-cursor-type-normal 'box)
  (setq meow-cursor-type-insert '(bar . 2))
  (setq meow-cursor-type-motion 'hollow)
  
  ;; 自定义模式行指示器
  (setq meow-replace-state-name-alist
        '((normal . "N")
          (insert . "I")
          (motion . "M")
          (keypad . "K")))
  
  ;; 启用全局模式
  (meow-global-mode 1))

;; 加载 pinyinlib
(use-package pinyinlib
  :load-path "~/.emacs.d/site-lisp/pinyinlib.el")

;; 加载 ace-pinyin
(use-package ace-pinyin
  :init
  ;; 使用 avy 作为后端
  (setq ace-pinyin-use-avy t)
  :config
  ;; 启用全局模式，支持中文拼音首字母跳转
  (ace-pinyin-global-mode +1))

;; Meow 与其他模式的集成
(with-eval-after-load 'meow
  ;; 为特定模式设置初始状态
  (add-to-list 'meow-mode-state-list '(pdf-view-mode . motion))
  (add-to-list 'meow-mode-state-list '(dired-mode . motion))
  (add-to-list 'meow-mode-state-list '(org-agenda-mode . motion))
  (add-to-list 'meow-mode-state-list '(magit-status-mode . motion))
  (add-to-list 'meow-mode-state-list '(helpful-mode . motion))
  (add-to-list 'meow-mode-state-list '(help-mode . motion))
  
  ;; 为 org-mode 添加特定键位
  (add-to-list 'meow-mode-state-list '(org-mode . normal))
  (meow-define-keys
   'normal
   '("TAB" . org-cycle))
  
  ;; 为 ace-pinyin 添加 meow 快捷键
  (with-eval-after-load 'ace-pinyin
    ;; 在普通模式下使用 'v' 键触发 ace-pinyin-char-2，'V' 键触发 ace-pinyin-in-line
    (meow-define-keys
     'normal
     '("v" . ace-pinyin-jump-char-2)
     '("V" . ace-pinyin-jump-char-in-line)))
  
  ;; 为 org-roam 添加特定键位
  (with-eval-after-load 'org-roam
    (meow-leader-define-key
     '("n f" . org-roam-node-find)
     '("n i" . org-roam-node-insert)
     '("n c" . org-roam-capture)
     '("n l" . org-roam-buffer-toggle))))

;; Meow 调试辅助函数
(defun meow-debug-info ()
  "显示 Meow 的调试信息。"
  (interactive)
  (let ((buf (get-buffer-create "*Meow Debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Meow 版本: %s\n" (if (fboundp 'meow-version)
                                         (meow-version)
                                       "未知")))
      (insert (format "当前状态: %s\n" meow--current-state))
      (insert (format "全局模式状态: %s\n" (if meow-global-mode "已启用" "未启用")))
      (insert "\n模式状态列表:\n")
      (dolist (mode-state meow-mode-state-list)
        (insert (format "  %s: %s\n" (car mode-state) (cdr mode-state))))
      (insert "\n键位映射:\n")
      (insert "  普通模式键位数量: ")
      (insert (format "%d\n" (length (cdr (assoc 'normal meow--kbd-alist)))))
      (insert "  插入模式键位数量: ")
      (insert (format "%d\n" (length (cdr (assoc 'insert meow--kbd-alist)))))
      (insert "  移动模式键位数量: ")
      (insert (format "%d\n" (length (cdr (assoc 'motion meow--kbd-alist)))))
      (insert "  Leader键位数量: ")
      (insert (format "%d\n" (length (cdr (assoc 'leader meow--kbd-alist))))))
    (switch-to-buffer buf)))

;; 绑定调试快捷键
(with-eval-after-load 'meow
  (meow-leader-define-key
   '("M-d" . meow-debug-info)))
