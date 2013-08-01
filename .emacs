;; Totally my .emacs!

;; Load stuff up.
(add-to-list 'load-path "~/.emacs.d/")
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install all the things.
(defconst packages-to-install
  '(coffee-mode
    flycheck
    magit
    markdown-mode
    yaml-mode
    zenburn-theme
    paredit
    linum
    undo-tree
    go-mode
    clojure-mode
    nrepl
    haskell-mode
    rainbow-delimiters))

;; Get all the packages.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(dolist (package packages-to-install)
  (when (not (package-installed-p package))
      (package-install package)))

;; Line numbering.
(require 'linum)
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
(require 'undo-tree)
(global-undo-tree-mode)

;; RAINBOW parentheses!
(require 'rainbow-delimiters)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

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

;; Go mode.
(require 'go-mode-load)

;; Common Lisp mode.
;; (add-to-list 'load-path "~/.emacs.d/slime-2012-03-05/")
;; (setq inferior-lisp-program "/usr/local/bin/lisp")
;; (require 'slime)
;; (slime-setup)

;; Racket for Scheming.
(setq scheme-program-name "racket")

;; Occur for useful regex searching.
(global-set-key (kbd "C-c o") 'occur)

;; 4 space tabs in ObjC mode.
(add-hook 'objc-mode-hook
	  '(lambda ()
	     (setq default-tab-width 4)))

;; Open corresponding header file in a split window.
(defun open-corresponding-header ()
  "Opens the corresponding header file for a .m or .c file, if it exists in the same directory."
  (interactive)
  (let ((current-file (split-string (buffer-name) "\\.")))
    (if (or (equal (cadr current-file) "m")
	    (equal (cadr current-file) "c")
	    (equal (cadr current-file) "cpp"))
	(if (file-exists-p (concat (car current-file) ".h"))
	    (find-file-other-window (concat (car current-file) ".h"))
	  (minibuffer-message "Corresponding header file doesn't exist!"))
      (minibuffer-message "This language probably doesn't use header files."))))

(global-set-key (kbd "C-c h") 'open-corresponding-header)


;; Automagically detect whether what type of .h we're editing. Due to Trey Jackson on Stack Overflow.
;; need find-file to do this
(require 'find-file)
;; find-file doesn't grok objc files for some reason, add that
;; (push ".m" (cadr (assoc "\\.h\\" cc-other-file-alist)))

(defun my-find-proper-mode ()
  (interactive)
  ;; only run on .h files
  (when (string-match "\\.h\\'" (buffer-file-name))
    (save-window-excursion
      (save-excursion
        (let* ((alist (append auto-mode-alist nil))  ;; use whatever auto-mode-alist has
               (ff-ignore-include t)                 ;; operate on buffer name only
               (src (ff-other-file-name))            ;; find the src file corresponding to .h
               re mode)
          ;; go through the association list
          ;; and find the mode associated with the source file
          ;; that is the mode we want to use for the .h file
          (while (and alist
                      (setq mode (cdar alist))
                      (setq re (caar alist))
                      (not (string-match re src)))
            (setq alist (cdr alist)))
          (when mode (funcall mode)))))))

(add-hook 'find-file-hook 'my-find-proper-mode)

;; Haskell mode.
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (turn-on-haskell-doc-mode)
	    (turn-on-haskell-indentation)
	    (setq require-final-newline t)))

;; Coffeescript mode.
;; (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
;; (require 'coffee-mode)

;; Clojure mode.
(require 'clojure-mode)

;; Use 256 colors in tmux.
(defun terminal-init-screen ()
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
(put 'upcase-region 'disabled nil)

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
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'inferior-scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'nrepl-mode-hook (lambda () (paredit-mode +1)))

;; (defun override-slime-repl-bindings-with-paredit ()
;;   (define-key slime-repl-mode-map
;;     (read-kbd-macro paredit-backward-delete-key) nil))

;; Turn off the menu bar.
(menu-bar-mode -1)
(put 'downcase-region 'disabled nil)

;; Set up running Clojure -- open a scratch buffer in Clojure mode,
;; and a REPL in a split window.
(defun setup-clojure ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (clojure-mode)
  (split-window-horizontally)
  (switch-to-buffer-other-window "*scratch*")
  (run-lisp "clj")
  (window-resize (selected-window) 50 :t)
  (switch-to-buffer-other-window "*scratch*"))

(global-set-key (kbd "C-c j") 'setup-clojure)

;; Format region nicely for Stack Overflow posting (i.e., 4 spaces
;; before each line) and copy it to the global clipboard (pbcopy).
(defun stack-overflow-format-region ()
  (interactive)
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
	 (split-text (split-string text "\n"))
	 (process-connection-type nil)
	 (proc (start-process "pbcopy" nil "pbcopy")))
    (process-send-string proc (concat "    \n"
				      (mapconcat 'identity
						 (map 'list
						      (lambda (str)
							(concat "    " str))
						      split-text) "\n")))
    (process-send-eof proc)))

;; Clojure REPL interaction.
(require 'nrepl)
(setq nrepl-popup-stacktraces nil)

;; Earmuff Lisp globals/dynamics/whatever.
;; Mostly from http://inclojurewetrust.blogspot.com/2011/11/earmuffs-and-variables.html, so thanks to that dude.
(defun earmuffy (&optional arg)
  (interactive "P")
  (let* ((variable (thing-at-point 'sexp))
	 (bounds (bounds-of-thing-at-point 'sexp))
	 (current-point (point))
	 (earmuffed-variable (concat "*" variable "*")))
    (save-excursion)
    (kill-region (car bounds) (cdr bounds))
    (if (and (string-equal (substring variable 0 1) "*")
	     (string-equal (substring variable (- 1)) "*"))
	;; unearmuffy
	(progn
	  (insert (substring variable 1 (- (length variable) 1)))
	  (goto-char (- current-point 1)))
      ;; earmuffy
      (progn
	(insert earmuffed-variable)
	(goto-char (+ current-point 1))))))

;; Easier window-moving controls.
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)

;; Bind align regexp to C-x a r
(global-set-key (kbd "C-x a r") 'align-regexp)

;; Always require a final newline.
(setq require-final-newline t)

;; Zenburn 2: color theme boogaloo
(load-theme 'zenburn t)

;; Conform with edX Coffeescript tab style.
(setq coffee-tab-width 2)

;; Keep tabs out forever.
(setq-default indent-tabs-mode nil)

;; Yaml.
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))

;; Make special file names use the right language mode.
(add-to-list 'auto-mode-alist '("^Cakefile$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^[Rr]akefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("^Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.underscore$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.make$" . html-mode))

;; Revert all buffers.
(defun revert-all-buffers ()
  "Revert all open buffers, if it makes sense to do so."
  (interactive)
  (dolist (b (buffer-list))
    (if (buffer-file-name b) ;; Only revert the file if it's associated with a file
      (condition-case nil
          (with-current-buffer b
            (revert-buffer nil t nil))
        (error nil))))
  (minibuffer-message "Reverted."))

(global-set-key (kbd "C-c r") 'revert-all-buffers)

(defun unsaved-buffer-list ()
  "List all unsaved buffers associated with files."
  (reduce (lambda (acc buffer)
            (if (and (buffer-modified-p buffer)
                     (buffer-file-name buffer))
                (cons (buffer-name buffer) acc)
              acc))
          (buffer-list)
          :initial-value '()))

(defun list-unsaved-buffers ()
  "Show the list of unsaved buffers."
  (interactive)
  (let ((unsaved-list (unsaved-buffer-list)))
    (if (> (length unsaved-list) 0)
        (with-output-to-temp-buffer "*Unsaved buffers*"
          (dolist (buffer unsaved-list)
            (princ (concat buffer "\n"))))
      (minibuffer-message "No unsaved buffers!"))))

;; Bind C-c d to delete around word.
(defun delete-around-word (&optional arg)
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (delete-region (car bounds) (cdr bounds))))

(global-set-key (kbd "C-c d") 'delete-around-word)

;; Markdown.
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
