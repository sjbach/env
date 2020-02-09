;;; -*- lexical-binding: t; -*-

(setq sh-basic-offset 2)  ;; default: 4

;; Spell check in comments.
;; Disabled; expensive in CPU and GC according to profiler.
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; Automatically break long lines.
(add-hook 'prog-mode-hook #'turn-on-auto-fill)
(add-hook 'prog-mode-hook #'steve-turn-on-fill-column-indiciator)

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
  ;; bindings, functions defined in macros (such as by cl-defstruct).
  (add-hook elisp-hook #'elisp-def-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq fill-column 79)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (flycheck-mode 1)))

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
