
;; cursor
(setq-default cursor-type 'bar) 

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; flx-ido
(flx-ido-mode 1)

;; remove welcome screen
(setq inhibit-startup-message t)

;; y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; stop system beep
(setq ring-bell-function (lambda () (message "*beep*")))

;; no backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; remove toolbar
(if window-system
    (tool-bar-mode 0))                          

;; open new emacs files in the same window
(setq ns-pop-up-frames nil)

;; maximize emacs
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; always reload buffer when file changes
(global-auto-revert-mode t)

;; custom theme for standalone Emacs
(if window-system
        (load-theme 'solarized-dark t))

;; use local defined ruby from rbenv
(rbenv-use-corresponding)
(setq rbenv-show-active-ruby-in-modeline t)

;; switch windows with Shift-arrow
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(windmove-default-keybindings 'super)

;; helm related bindings
(global-set-key (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x g") 'helm-google-suggest)



