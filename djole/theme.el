;;; theme

(setq custom-safe-themes t)

(if window-system
    (add-hook 'after-init-hook
	      (load-theme 'kaolin-dark)))

;; ample
;; base16-solarflare
;; base16-materia
;; solarized-darka
;; base16-woodland
;; base16-atelier-cave (mozda)
;; base16-atelier-savanna
;; base16-bespin

(provide 'theme)
