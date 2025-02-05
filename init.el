;; 基本设置文件 - 加载主配置
(setq package-enable-at-startup nil)  ; 防止package.el在启动时初始化两次
(package-initialize)

;; 加载org格式的配置文件
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; 自定义变量（由customize生成的部分保留在此）
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
