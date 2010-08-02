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
(require 'vimpulse)
(require 'viper-in-more-modes)

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

;; Remove read-only property from pasted text -- in newer versions of slime,
;; output to slime-repl is read-only and I often want to modify text I copied
;; from there.
(dolist (fn '(viper-put-back viper-Put-back))
  (eval `(defadvice ,fn (around steve-remove-read-only activate)
           (let ((text
                  ;; Taken from viper-put-back
                  (if viper-use-register
                      (cond ((viper-valid-register viper-use-register '(digit))
                             (current-kill
                              (- viper-use-register ?1) 'do-not-rotate))
                            ((viper-valid-register viper-use-register)
                             (get-register (downcase viper-use-register)))
                            (t (error viper-InvalidRegister viper-use-register)))
                      (current-kill 0))))
             (when text
               (put-text-property 0 (length text) 'read-only nil text)))
           ad-do-it)))

;; S-expression text objects
(vimpulse-define-text-object vimpulse-sexp (arg)
  "Select an S-expression."
  :keys '("ae" "ie")
  (vimpulse-inner-object-range
   arg
   'backward-sexp
   'forward-sexp))

;; Comment/uncomment Visual selection with ,c
(vimpulse-vmap ",c" 'comment-dwim)

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

