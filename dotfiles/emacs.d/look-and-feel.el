;;
;; Look and feel
;;
(require 'color-theme)
;(color-theme-xemacs)
(color-theme-initialize)
(let ((display (getenv "DISPLAY")))
  (if (and display (> (length display) 0))
      ;; X
      (color-theme-robin-hood)
    ;; Terminal
    (color-theme-comidia)))
;(color-theme-sitaramv-solaris)

; Slime autodoc often expands the minibuffer height, which is annoying.
(setq resize-mini-windows nil)

;; Different color for parens
(require 'parenface)

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq inhibit-startup-message t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;(set-default-font "Bitstream Vera Sans Mono-8")
;(set-default-font "Bitstream Vera Sans Mono-9")
(set-default-font "Bitstream Vera Sans Mono-10")
;(set-default-font "Bitstream Vera Sans Mono-11") 
;(set-default-font "Bitstream Vera Sans Mono-12")

;; Turn on font-lock mode for syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Show colums and lines in the status bar
(column-number-mode t)
(line-number-mode 1)

;; Enable visual feedback on selections
(transient-mark-mode t)

(set-scroll-bar-mode 'right)

;; The completion buffer still shows too much boilerplate, but this helps
(setq completion-show-help nil)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; Prevent the annoying beep on errors
(setq visible-bell t)

;; Highlight XXX style code tags in source
(let ((words
       '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|BUG\\|STEVE\\)"
          1 font-lock-warning-face prepend))))
  (font-lock-add-keywords 'python-mode words)
  (font-lock-add-keywords 'c-mode words)
  (font-lock-add-keywords 'c++-mode words)
  (font-lock-add-keywords 'lisp-mode words)
  (font-lock-add-keywords 'emacs-lisp-mode words))

