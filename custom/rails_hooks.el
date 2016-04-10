
;; projectile
(define-key projectile-mode-map (kbd "s-a") 'projectile-find-file)

;; projectile-rails
(define-key projectile-mode-map (kbd "s-m") 'projectile-rails-find-model)
(define-key projectile-mode-map (kbd "s-r") 'projectile-rails-find-controller)
(define-key projectile-mode-map (kbd "s-t") 'projectile-rails-find-spec)
(define-key projectile-mode-map (kbd "s-j") 'projectile-rails-find-javascript)

;; rspec-mode
(define-key rspec-mode-map (kbd "C-q a") 'rspec-verify-all)
(define-key rspec-mode-map (kbd "C-q b") 'rspec-verify-matching)
(define-key rspec-mode-map (kbd "C-q q") 'rspec-verify-single)
(define-key rspec-mode-map (kbd "C-q f") 'rspec-run-last-failed)
;; special case, connected to projectile (when on model, rspec is not loaded)
(define-key projectile-mode-map (kbd "C-q <tab>") 'rspec-toggle-spec-and-target)






