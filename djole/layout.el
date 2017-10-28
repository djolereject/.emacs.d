;;; layout


;; set cursor
(setq-default cursor-type 'bar)

;; remove welcome screen
(setq inhibit-startup-message t)

;; remove toolbar
(tool-bar-mode 0)

;; remove scroll bars
(scroll-bar-mode 0)

;; maximize emacs
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; show y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; stop system beep
(setq ring-bell-function (lambda () (message "*beep*")))

(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'text-mode)

;; no gui tooltips
(setq tooltip-use-echo-area t)

;; linum mode just for programming
(add-hook 'prog-mode-hook 'linum-mode)

;; number mode always
(column-number-mode t)

;; highlight active line
;;(global-hl-line-mode t)

(provide 'layout)
