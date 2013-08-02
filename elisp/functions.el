;; Functions I've defined.

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

;; Use 256 colors in tmux.
(defun terminal-init-screen ()
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
(put 'upcase-region 'disabled nil)


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
