;;;; orgmode

(add-hook 'org-mode-hook #'org-bullets-mode 1)

(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))


;; not sure if it's needed, maybe I could completely remove pandoc
(with-eval-after-load 'ox
  (require 'ox-pandoc))

;; alternative style for pdf output
(with-eval-after-load 'ox-pandoc
   (add-to-list 'org-latex-classes
          '("koma-article"
             "\\documentclass{scrartcl}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(provide 'orgmode)
