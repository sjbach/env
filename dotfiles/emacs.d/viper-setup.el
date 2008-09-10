;;;
;;; Vim-like stuff
;;; Also see:
;;;   ~/.viper
;;;   ~/.elisp/vimpulse.el
;;;   ~/.elisp/viper-in-more-modes.el
;;;

(setq viper-mode t)
(require 'viper)
(require 'vimpulse)
(require 'viper-in-more-modes)
(require 'redo)
(require 'rect-mark)  ; For block visual mode.

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
