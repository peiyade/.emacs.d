(setq package-enable-at-startup nil)
(setq package-archives '(("gnu"   . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "http://mirrors.163.com/elpa/melpa/")
                         ("org"   . "http://orgmode.org/elpa/")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(scroll-bar-mode -1)

(add-to-list 'load-path "~/.emacs.d/lisp/bui.el")
(require 'bui)

(defun buffers-buffer->entry (buffer)
  (with-current-buffer buffer
    `((id   . ,buffer)
      (name . ,(buffer-name))
      (mode . ,major-mode)
      (size . ,(buffer-size))
      (file-name . ,buffer-file-name))))

(defun buffers-get-entries ()
  (mapcar 'buffers-buffer->entry (buffer-list)))

(bui-define-interface buffers list
  :buffer-name "*Buffers*"
  :get-entries-function 'buffers-get-entries
  :format '((name nil 30 t)
            (mode nil 25 t)
            (size nil 8 bui-list-sort-numerically-2 :right-align t)
            (file-name bui-list-get-file-name 30 t))
  :sort-key '(name))

(defun buffers ()
  "Display a list of buffers."
  (interactive)
  (bui-get-display-entries 'buffers 'list))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(cnfonts-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp/avy")
(require 'avy)

(add-to-list 'load-path "~/.emacs.d/lisp/emacsql/")
(require 'emacsql)

(recentf-mode 1)
(global-set-key (kbd "C-x p") 'recentf-open-files)
(global-set-key [f8] 'recentf-open-files)

(add-to-list 'load-path "~/.emacs.d/lisp/yasnippet")
(require 'yasnippet)
(add-to-list 'load-path "~/.emacs.d/lisp/auto-yasnippet")
(require 'auto-yasnippet)
(add-to-list 'load-path "~/.emacs.d/lisp/yasnippet-snippets")
(require 'yasnippet-snippets)

(yas-global-mode +1)
(global-set-key (kbd "C-c i") 'yas-insert-snippet)

(defun indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-M-\\") 'indent-buffer)

(add-to-list 'load-path "~/.emacs.d/lisp/ace-window")
(require 'ace-window)
(setq aw-dispatch-always t)
(global-set-key (kbd "C-x o") 'ace-window)
(defvar aw-dispatch-alist
  '((?d aw-delete-window "Delete Window")
    (?D delete-other-windows "Delete Other Windows")
    (?m aw-swap-window "Swap Windows")
    (?M aw-move-window "Move Window")
    (?c aw-copy-window "Copy Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window "Flip Window")
    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
    (?f aw-split-window-fair "Split Fair Window")
    (?h aw-split-window-vert "Split Vertical")
    (?v aw-split-window-horz "Split Horizontal")
    (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

(global-set-key [f7] 'toggle-input-method)

(add-to-list 'load-path
             "~/.emacs.d/lisp/emacs-rime")
(require 'rime)

(setq rime-user-data-dir "~/.config/fcitx5/rime")
(setq default-input-method "rime")

(setq rime-show-candidate 'posframe)

(add-to-list 'load-path
             "~/.emacs.d/lisp/posframe")
(require 'posframe)

(add-to-list 'load-path "~/.emacs.d/lisp/auctex")

(setq org-hide-leading-stars t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets '(("projects.org" . (:level . 1))))

(add-to-list 'load-path "~/.emacs.d/lisp/doct/")
(require 'doct)

(add-to-list 'load-path "~/.emacs.d/lisp/org-protocol-capture-html/")
(require 'org-protocol-capture-html)

(add-to-list 'load-path "~/.emacs.d/lisp/deft/")
(require 'deft)

(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/braindump")
(setq deft-recursive t)
(setq deft-use-filename-as-title t)
(setq deft-strip-summary-regexp
      (concat "\\("
              "^:.+:.*\n" ; any line with a :SOMETHING:
              "\\|^#\\+.*\n" ; anyline starting with a #+
              "\\|^\\*.+.*\n" ; anyline where an asterisk starts the line
              "\\)"))
(global-set-key [f3] 'deft)

(add-to-list 'load-path "~/.emacs.d/lisp/org-roam/")
(require 'org-roam)

(setq org-roam-directory (file-truename "~/braindump"))

(setq org-roam-db-location (file-truename "~/braindump/org-roam.db"))
(setq org-roam-v2-ack t)
(org-roam-db-autosync-mode)

(setq org-roam-keymap
      '( keymap 
         ( ?m . org-roam-mode)		           ;m
         ( 59 . org-roam-buffer-toggle)	           ;;
         ( ?: . org-roam-buffer-display-dedicated) ;:
         ( ?c . org-roam-capture)		   ;c
         ( ?' . org-roam-alias-add)		   ;'
         ( ?r . org-roam-node-random)		   ;r
         ( ?g . org-roam-graph)		   ;g
         ( ?d  keymap				   ;d
           ( ?d . org-roam-dailies-capture-date)  ;dd
           ( ?t . org-roam-dailies-capture-today) ;dt
           ( ?f . org-roam-dailies-find-directory)) ;df
         ( ?t . org-roam-tag-add)		    ;t
         ( ?T . org-roam-tag-delete)		    ;T
         ( ?f . org-roam-node-find)		    ;f
         ( ?i . org-roam-node-insert)		    ;i
         ))

;; Inbox
(setq pei/inbox-file "~/braindump/inbox.org")
(setq org-capture-templates
      `(("i" "inbox" entry (file pei/inbox-file)
         "* TODO %?")
        ("w" "Web site" entry (file pei/inbox-file)
         "* %a :website:\n\n%U %?\n\n%:initial")))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
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
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   (cons "r" org-roam-keymap))
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
   '("z" . meow-pop-selection)))

(add-to-list 'load-path
             "~/.emacs.d/lisp/meow")
(require 'meow)
(setq meow-keyboard-describe-delay 0.1)
(setq meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi)
(meow-setup)
(meow-setup-indicator)
(meow-global-mode 1)
