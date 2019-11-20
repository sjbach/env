;;; -*- lexical-binding: t; -*-
;; Mostly taken from:
;;
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;
; M-x package-install cargo
; M-x package-install company
; M-x package-install racer
; M-x package-install flycheck-rust

(require 'fill-column-indicator)

;; Cargo:
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            ; STEVE vv untested
            ; (setq rust-format-on-save t)
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
(add-hook 'cargo-process-mode-hook 'visual-line-mode) ; soft wrap

(add-hook 'rust-mode-hook #'racer-mode)
;; Display annotations in the minibuffer.
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'rust-mode-hook #'rainbow-delimiters-mode)

;; Flycheck-rust:
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)

;; Fill-column:
(add-hook 'rust-mode-hook #'steve-turn-on-fill-column-indiciator)
(add-hook 'rust-mode-hook
          ;; 100 character lines, per style guide.
          ;; (99 to be conservative.)
          (lambda () (setq fill-column 99)))

;; Misc:
; (setenv "RUST_BACKTRACE" "1")
(setenv "RUST_BACKTRACE" "full")

