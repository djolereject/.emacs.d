;;;; My hooks for making life easier

;; copy line 
;;
(defun copy-line (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))



;; rename buffer connected to file (Yegge)
;;
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
	(filename (buffer-file-name)))
 (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (if (get-buffer new-name)
	 (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) ;;



;; move buffer connected to file  (Yegge)
;;
(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))

(if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
 (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 


;; key bindings
;;
(global-set-key (kbd "\C-c c") 'copy-line)
(global-set-key (kbd "\C-c C-r") 'rename-file-and-buffer)
(global-set-key (kbd "\C-c C-m") 'move-buffer-file)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; cycle through buffers
(global-set-key (kbd "<C-tab>") 'bury-buffer)


;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-rsense)))
