
;; Set initial file
(defun open-init-file() ;; initial file define.
  (interactive)
  (find-file "~/.emacs.d/settings.org"))
;; Use <F2> to visit initial file
(global-set-key (kbd "<f2>") 'open-init-file)


(require 'org)
(setq user-emacs-directory "~/.emacs.d/")
     
;; .el -> .org
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))




