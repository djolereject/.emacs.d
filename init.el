;;;; Init file for emacs
;; This is for loading basics only, most of the job is done in settings.org

;; Package.el
;;
(package-initialize)

;; Preload config and data for other packages
;;
(setq config-dir (expand-file-name "config/" user-emacs-directory))
(setq data-dir (expand-file-name "data/" user-emacs-directory))

;; Create directories if they don't exist
;;
(unless (file-exists-p data-dir) (make-directory data-dir))
(unless (file-exists-p config-dir) (make-directory config-dir))

;; Use babel to compile all code from settings.org
;;
(setq vc-follow-symlinks nil)
(org-babel-load-file (expand-file-name "settings.org" user-emacs-directory))
