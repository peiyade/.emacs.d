
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




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Sarasa Mono SC Nerd" :foundry "????" :slant normal :weight regular :height 98 :width normal)))))
