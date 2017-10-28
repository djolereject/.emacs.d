;;; global

;; ido mode setup
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode t)

;; smex setup (replace M-x)
(global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; revert buffers on external changes
(global-auto-revert-mode t)

 ;; no backups
(setq make-backup-files nil)
(setq auto-save-default nil)

;; for osx
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "s-w") nil)

;; snap to height of screen
(setq frame-resize-pixelwise t)

;; use pandoc as markdown's external converter
(setq markdown-command "pandoc")


(provide 'global)
