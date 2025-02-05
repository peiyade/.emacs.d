(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq visible-bell t)

(set-language-environment "UTF-8")
(setq default-buffer-file-coding-system 'utf-8)

(setq package-archives
      '(("gnu"   . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
        ("org"   . "http://mirrors.cloud.tencent.com/elpa/org/")))

(require 'package)
(unless package-archive-contents
  (package-refresh-contents))

;; 自动安装 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(set-face-attribute 'default nil :font "Monaco-14")
(set-fontset-font t 'han (font-spec :name "PingFang SC"))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/org-roam")
  (org-roam-db-location "~/Documents/org-roam/org-roam.db")
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
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
        "${title:*} ${tags:10}")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain "%?"
           :if-new (file+head "references/${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+TAGS: reference\n\n")
           :unnarrowed t)))
  (setq org-roam-tag-sources '(prop last-directory))
  (setq org-roam-file-exclude-regexp ".*/archive/.*\\.org$"))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-engine 'xetex)
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-export-with-sub-superscripts '{})
  (setq org-src-fontify-natively t)
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))
  (setq org-latex-default-class "ctexart")
  (setq org-format-latex-options '(:foreground default :background default :scale 2.0)))

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq pdf-view-resize-factor 1.1)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package company
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1))

(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq default-input-method "MacOSX")
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format "%b"))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

(setq gc-cons-threshold (* 50 1000 1000))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))
