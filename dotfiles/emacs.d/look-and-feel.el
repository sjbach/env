;;; -*- lexical-binding: t; -*-
;;
;; Look and feel
;;

;; Scrolling behavior. (Make similar to Vim)
;;
;; Begin scrolling the window four lines before the margin.
(setq scroll-margin 4)
;; When point moves out of the window, don't recenter the window on point,
;; rather scroll just enough to get point in the window again (respecting the
;; margin above).
(setq scroll-conservatively 200)  ; (arbitrary high number)

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tooltip-mode -1)

(require 'diminish)
(diminish 'undo-tree-mode)
(diminish 'company-mode)
(diminish 'git-gutter-mode)
(diminish 'eldoc-mode)
(diminish 'elisp-slime-nav-mode)
(diminish 'which-key-mode)
(diminish 'racer-mode)
(diminish 'cargo-minor-mode)
(diminish 'hs-minor-mode)

;; Window splitter styling (for vertical splits)
(set-face-inverse-video 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "blue")  ;; magenta?
;; Set a nicer symbol than "|"
(require 'disp-table)  ;; ensure standard-display-table is set
(defun steve-set-smooth-window-divider ()
  (let ((display-table (or buffer-display-table
                           (window-display-table)
                           standard-display-table)))
    (when display-table
      (set-display-table-slot
       ;; Thinner alternative: ?│
       display-table 'vertical-border (make-glyph-code ?┃))
      (set-window-display-table (selected-window) display-table))))
(add-hook 'window-configuration-change-hook #'steve-set-smooth-window-divider)

;; Terminal title
(when (boundp 'xterm-set-window-title)
  (setq xterm-set-window-title t))

;; Consider:
;; (global-display-fill-column-indicator-mode 1)

(require 'git-gutter)
(global-git-gutter-mode 1)

(require 'company)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 30)
;; Lisp
;; A little extra highlighting in Lisp.
(require 'lisp-extra-font-lock)
(lisp-extra-font-lock-global-mode 1)
;;

;; Ace window
(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Which-key
(require 'which-key)
(setq which-key-sort-order 'which-key-prefix-then-key-order)
(setq which-key-allow-evil-operators t)
(setq which-key-idle-delay 0.25)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-is-verbose t)
(setq which-key-side-window-max-width 0.5)
(which-key-mode 1)
;; (which-key-setup-side-window-bottom)
(which-key-setup-side-window-right)

;; Show colums and lines in the status bar
(column-number-mode t)
(line-number-mode 1)

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
  (font-lock-add-keywords 'c-mode words)
  (font-lock-add-keywords 'c++-mode words)
  (font-lock-add-keywords 'emacs-lisp-mode words)
  (font-lock-add-keywords 'ess-mode words)
  (font-lock-add-keywords 'html-mode words)
  (font-lock-add-keywords 'java-mode words)
  (font-lock-add-keywords 'js-mode words)
  (font-lock-add-keywords 'latex-mode words)
  (font-lock-add-keywords 'lisp-mode words)
  (font-lock-add-keywords 'nxml-mode words)
  (font-lock-add-keywords 'python-mode words)
  (font-lock-add-keywords 'ruby-mode words)
  (font-lock-add-keywords 'sh-mode words)
  (font-lock-add-keywords 'rust-mode words))

(when (string-equal system-type "darwin")
  (toggle-frame-maximized))

