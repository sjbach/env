;;; -*- lexical-binding: t -*-

(require 'company)
;; `company-dabbrev' can be distracting as it will try to complete words inside
;; comments. Both the menu and the a minor computational drag on the UI.
;; `company-dabbrev-code' is still included.
(setq-default company-backends
              (remove 'company-dabbrev company-backends))

;; Fire the martinet.
(setq-default flycheck-disabled-checkers
              '(emacs-lisp-checkdoc))

;; Treat underscore as a word character rather than a syntax character unless
;; otherwise specified.
(modify-syntax-entry ?_ "w" (standard-syntax-table))

(setq sh-basic-offset 2)  ;; default: 4

;; Spell check in comments.
;; Disabled; expensive in CPU and GC according to profiler.
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; Automatically break long comment lines.
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook #'steve-turn-on-fill-column-indiciator)

;; Highlight trailing whitespace when it's actionable.
(add-hook 'prog-mode-hook #'steve-maybe-highlight-trailing-whitespace)

(add-hook 'html-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (set-fill-column 100)))
(add-hook 'sgml-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (set-fill-column 100)))
(add-hook 'python-mode-hook
          (lambda () (set-fill-column 80)))
(add-hook 'c++-mode-hook
          (lambda () (set-fill-column 80)))

(add-hook 'sh-mode-hook
          (lambda () (flycheck-mode 1)))

(dolist (elisp-hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook elisp-hook #'turn-on-elisp-slime-nav-mode)
  (add-hook elisp-hook #'company-mode)
  ;; (setq rainbow-delimiters-max-face-count 8)  ;; vs 9
  (add-hook elisp-hook #'rainbow-delimiters-mode)
  (add-hook elisp-hook #'highlight-parentheses-mode)
  ;; Better version of `xref-find-definitions` or
  ;; `elisp-slime-nav-find-elisp-thing-at-point` that works on e.g. local
  ;; variable bindings and functions defined in macros (such as by
  ;; cl-defstruct).
  (add-hook elisp-hook #'elisp-def-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq fill-column 79)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Don't try to make sense of scratch buffers.
            (unless (or (string-equal (buffer-name) "*scratch*")
                        (string-equal (buffer-name) "*Scratch*"))
            (flycheck-mode 1))))

;; Emacs Lisp code can become hard to scan because of the heavy use of package
;; prefixes resulting from the absence of true namespacing in the language.
;; `nameless-mode' helps ameliorate this.
;;
;; package-foo becomes :foo and package--bar becomes ::bar.
(setq nameless-prefix ":")        ; default: ":"
;; A middot is an unambiguous token but it interacts oddly with `fci-mode' and
;; with the window divider glyph in terminal Emacs.
;; (setq nameless-prefix "·")        ; default: ":"
;; (setq nameless-prefix "/")        ; default: ":"
(setq nameless-private-prefix t)  ; default: nil
;; Indent code and comments according to the true content, not the displayed
;; content, as most readers will not be using `nameless-mode'.
(setq nameless-affect-indentation-and-filling nil)  ; default: 'outside-strings
;; (Note: `nameless-face' also customized to tamp down its vibrancy.)

(defun steve-turn-on-fill-column-indiciator ()
  (if (fboundp 'display-fill-column-indicator-mode)
      ;; Only present in 27.1+.
      (display-fill-column-indicator-mode 1)
    (turn-on-fci-mode)))

(defun steve-maybe-highlight-trailing-whitespace ()
  (unless (or buffer-read-only
              (and (buffer-file-name)
                   (string-match-p "emacs\.d/elpa/"
                                   (buffer-file-name))))
    (setq show-trailing-whitespace t)))

;; ^L is used as a section separator in GNU code. Make it look purposeful.
(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.
URL `http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html'
Version 2018-08-30"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    ;; (not necessary, and causes flicker)
    ;; (redraw-frame)
    ))
(add-hook 'emacs-lisp-mode-hook #'xah-show-formfeed-as-line)

(defun steve--add-fixme-font-lock-keywords ()
  "Highlight XXX-style tokens in source code."
  (let* ((tokens '("FIXME" "HACK" "XXX" "TODO" "BUG" "STEVE"))
         (regex (regexp-opt tokens 'words))
         (font-lock-stanza `((,regex 0 font-lock-warning-face prepend))))
    (font-lock-add-keywords nil font-lock-stanza)))
;;
(dolist (hook '(prog-mode-hook sgml-mode-hook))
  (add-hook hook #'steve--add-fixme-font-lock-keywords))

