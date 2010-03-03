;;;
;;; Vim-like stuff
;;; Also see:
;;;   ~/.emacs.d/dot-viper.el
;;;   ~/.elisp/vimpulse.el
;;;   ~/.elisp/viper-in-more-modes.el
;;;

(setq viper-mode t)
(setq viper-custom-file-name
      (convert-standard-filename "~/.emacs.d/dot-viper.el"))
(require 'viper)
(require 'vimpulse)
(require 'viper-in-more-modes)
(require 'redo)
(require 'rect-mark)  ; For block visual mode.

;; Hack to get *Messages* in viper-mode.
;; (must be done after loading viper)
;; Futzing with fundamental-mode doesn't seem to help.
(save-excursion
  (set-buffer "*Messages*")
  (viper-change-state-to-vi))

;; Make Emacs mode stick out more in status bar.
(setq viper-emacs-state-id
      (concat (propertize "<EMACS>" 'face 'isearch) " "))
;(setq viper-vi-state-id
;      (concat (propertize "<V>" 'face 'holiday-face) " "))
;(setq viper-insert-state-id
;      (concat (propertize "<I>" 'face 'holiday-face) " "))
;(setq viper-replace-state-id
;      (concat (propertize "<R>" 'face 'holiday-face) " "))

;; the property `risky-local-variable' is a security measure for mode line
;; variables that have properties.
(put 'viper-mode-string 'risky-local-variable t)

; Show when minibuffer is in Emacs mode.
(when (facep 'viper-minibuffer-emacs)
  (set-face-foreground 'viper-minibuffer-emacs "white")
  (set-face-background 'viper-minibuffer-emacs "black"))

;; Viper is overreaching by caring whether a visited file is under version
;; control -- disable this check.
(defadvice viper-maybe-checkout (around viper-vcs-check-is-retarded activate)
  nil)

;; Let ESC disable visual mode.
(defadvice viper-intercept-ESC-key (around steve-visual-mode-ESC activate)
  (if vimpulse-visual-mode
    (vimpulse-visual-mode 'toggle)
    ad-do-it))

;; Simple Vim command line functions

(defun w (&optional args)
  (interactive "p")
  (save-buffer args))

(defun q (&optional args)
  (interactive "P")
  (save-buffers-kill-emacs args))

(defun wq (&optional args)
  (interactive "P")
  (save-buffers-kill-emacs args))

