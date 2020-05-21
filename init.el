;;;; Init file for emacs
;; This is for loading basics only, most of the job is done in settings.org

;; Package.el
;;
(if (version< emacs-version "27")
    (package-initialize))

;; Preload config and data for other packages
;;
(setq config-dir (expand-file-name "config/" user-emacs-directory))
(setq data-dir (expand-file-name "data/" user-emacs-directory))

;; Create those directories if they don't exist
;;
(unless (file-exists-p data-dir) (make-directory data-dir))
(unless (file-exists-p config-dir) (make-directory config-dir))

;; Set global variables for README.org file and config/settings.el, one is original source of code and the other one is what babel tangles
;;
(setq original-source (expand-file-name "README.org" user-emacs-directory))
(setq compiled-source (expand-file-name "settings.el" config-dir))

;; `org-babel-load-file` doesn't work anymore when destination file is different that original with .el extension, something changed with Emasc 27 or org.
;; `org-babel-tangle-file` and `load-file` are replacing that function.
;;
(require 'org)
(org-babel-tangle-file original-source compiled-source)
(load-file compiled-source)
