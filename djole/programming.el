;;; programming
(require 'ruby-mode)
(require 'inf-ruby)
(require 'rspec-mode)
(require 'ruby-tools)
(require 'yard-mode)

;; Files with the following extensions should open in ruby-mode
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))


(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'robe-mode)

;; Rubocop binding
(add-hook 'ruby-mode-hook #'rubocop-mode)
;; I want rspec instead of rake spec
(setq rspec-use-rake-when-possible nil)
;; Scroll to the first test failure
(setq compilation-scroll-output 'first-error)
;; Automatic save
(setq compilation-ask-about-save nil)
;; Rspec gets into editing mode on pry
(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

;; move cursor by camelCase
(add-hook 'ruby-mode-hook 'subword-mode 1)

(provide 'programming)
