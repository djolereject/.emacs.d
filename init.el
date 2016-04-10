;;;; emacs configuration

;; All packages are managed by cask
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

;; Automatically add all packages to Cask file 
(require 'pallet)
(pallet-mode t)

;; My files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom"))

(load "global.el")
(load "custom_hooks.el")
(load "rails.el")
(load "rails_hooks.el")
(load "front_end.el")

