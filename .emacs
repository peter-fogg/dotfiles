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
    rainbow-delimiters
    skewer-mode))

;; Get all the packages.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(dolist (package packages-to-install)
  (when (not (package-installed-p package))
      (package-install package)))

;; Load up custom functions.
(load-file "elisp/functions.el")
(load-file "elisp/hooks.el")
(load-file "elisp/initialization.el")

;; The most important part of this file.
(load-theme 'zenburn t)
