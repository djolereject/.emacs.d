;;;; emacs configuration

;; All packages are managed by cask


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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
(require 'custom_hooks)
(require 'keyboard)

