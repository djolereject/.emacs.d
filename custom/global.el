
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

(setq ag-highlight-search t)

;;;; Display ido results vertically, rather than horizontally
;; (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; (defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
;; (add-hook 'ido-setup-hook 'ido-define-keys)

