;;; keyboard

;; helm related bindings
(global-set-key (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x g") 'helm-google-suggest)


;; combine projectile and helm
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(define-key projectile-mode-map (kbd "C-c") #'projectile-command-map)

;; rspec-mode
(define-key rspec-mode-map (kbd "C-q a") 'rspec-verify-all)
(define-key rspec-mode-map (kbd "C-q b") 'rspec-verify-matching)
(define-key rspec-mode-map (kbd "C-q q") 'rspec-verify-single)
(define-key rspec-mode-map (kbd "C-c C-c") 'rspec-verify-single)
(define-key rspec-mode-map (kbd "C-q f") 'rspec-run-last-failed)

;; key bindings from custom.el
(global-set-key (kbd "C-. c") 'copy-line)
(global-set-key (kbd "C-. r") 'rename-file-and-buffer)
(global-set-key (kbd "C-. m") 'move-buffer-file)

;; ace-window
(global-set-key (kbd "M-p") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


(provide 'keyboard)
