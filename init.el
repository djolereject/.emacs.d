;;;; emacs configuration

;; All packages are managed by cask
;;(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; Automatically add all packages to Cask file
(require 'pallet)
(pallet-mode t)

;; Load customizations on init
(defvar djole-dir (expand-file-name "djole" user-emacs-directory)
  "Customization directory.")

(add-to-list 'load-path djole-dir)
(require 'layout)
(require 'theme)
(require 'global)
(require 'programming)
(require 'orgmode)
(require 'custom_hooks)
(require 'keyboard)


