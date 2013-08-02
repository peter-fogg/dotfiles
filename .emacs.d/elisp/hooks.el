;; Various mode hooks.

;; Rainbow-delimiters.
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;; 4 space tabs in ObjC mode.
(add-hook 'objc-mode-hook
	  '(lambda ()
	     (setq default-tab-width 4)))

;; Haskell.
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (turn-on-haskell-doc-mode)
	    (turn-on-haskell-indentation)
	    (setq require-final-newline t)))

;; Paredit.
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'inferior-scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'nrepl-mode-hook (lambda () (paredit-mode +1)))

;; Skewer.
(add-hook 'js2-mode-hook 'skewer-mode)
