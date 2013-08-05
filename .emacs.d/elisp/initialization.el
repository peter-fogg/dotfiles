;; Initialization for various modes.

;; Line numbering.
(global-linum-mode 1)
(setq linum-format "%d ")

;; Transient mark mode.
(setq-default transient-mark-mode 1)

;; Show parentheses.
(setq show-paren-delay 0)
(show-paren-mode)
(set-face-background 'show-paren-match-face "#8cd0d3")
(set-face-attribute 'show-paren-match-face nil :weight 'bold :underline nil :overline nil :slant 'normal)

;; Don't show startup message, etc.
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; Put backups in a special directory.
(setq backup-directory-alist '(("." . "~/.backup_saves")))
(setq auto-save-file-name-transforms `((".*" ,"~/.backup_saves" t)))

;; Undo-tree mode.
(global-undo-tree-mode)

;; Bind hippie-expand to M-/
(global-set-key "\M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

;; Racket for Scheming.
(setq scheme-program-name "racket")

;; C# mode.
;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist
;;       (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; (defun set-csharp-mode-tabs ()
;;   (require 'flymake)
;;   (setq indent-tabs-mode t)
;;   (c-set-style "C#")
;;   (setq c-basic-offset 8)
;;   (setq tab-width 8))
;; (add-hook 'csharp-mode-hook 'set-csharp-mode-tabs)

;; R mode.
;; (load "~/.emacs.d/ess-12.04-4/lisp/ess-site")

;; Paredit mode stuff.
(autoload 'paredit-mode "paredit")

;; (defun override-slime-repl-bindings-with-paredit ()
;;   (define-key slime-repl-mode-map
;;     (read-kbd-macro paredit-backward-delete-key) nil))

;; Turn off the menu bar.
(menu-bar-mode -1)
(put 'downcase-region 'disabled nil)

;; Clojure nrepl interaction.
(setq nrepl-popup-stacktraces nil)

;; Easier window-moving controls.
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)

;; Always require a final newline.
(setq require-final-newline t)

;; Conform with edX Coffeescript tab style.
(setq coffee-tab-width 2)

;; Keep tabs out forever.
(setq-default indent-tabs-mode nil)

;; Make special file names use the right language mode.
(add-to-list 'auto-mode-alist '("^Cakefile$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^[Rr]akefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.underscore$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . html-mode))

;; Markdown.
(add-to-list 'auto-mode-alist '("\\.text$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Use JS2 mode for JS.
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Don't spam SASS compilation.
(setq scss-compile-at-save nil)
