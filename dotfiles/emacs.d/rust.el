;; Mostly taken from:
;;
;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;
; M-x package-install cargo
; M-x package-install company
; M-x package-install racer
; M-x package-install flycheck-rust

;; Cargo:
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            ; STEVE vv untested
            ; (setq rust-format-on-save t)
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; Racer:
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/rust/src")
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; Flycheck-rust:
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'flycheck-mode)

;; Misc:
; (setenv "RUST_BACKTRACE" "1")
(setenv "RUST_BACKTRACE" "full")

