;;;
;;; Vim-like stuff
;;; Also see:
;;;   ~/.emacs.d/viper-setup.el
;;;   ~/.elisp/vimpulse.el
;;;   ~/.elisp/viper-in-more-modes.el
;;;

(setq-default viper-inhibit-startup-message t)
(setq-default viper-expert-level '5)
(setq-default viper-want-ctl-h-help t)
(setq-default viper-ESC-moves-cursor-back t)
(setq-default viper-auto-indent t)
(setq-default viper-keep-point-on-repeat nil)
(setq-default viper-toggle-key [C-up])
(setq-default viper-ESC-keyseq-timeout 0)
(setq-default viper-translate-all-ESC-keysequences nil)
(setq-default viper-change-notification-threshold 200)

; Allow backspace past start of edit and beginning of line.
(setq-default viper-ex-style-editing nil)  

; Non-sluggish paren matching (using "%" key).
(viper-set-parsing-style-toggling-macro 'undefine)

; Vimpulse/Viper modification
(define-key viper-insert-global-user-map "\C-[" 'viper-intercept-ESC-key)
(define-key viper-insert-global-user-map "\C-g" 'viper-intercept-ESC-key)
(define-key viper-vi-global-user-map "\C-d"
                                     'close-buffer-and-window-unless-last)
(define-key viper-vi-global-user-map "\C-y" (lambda ()
                                              (interactive)
                                              (viper-scroll-down-one 3)))
(define-key viper-vi-global-user-map "\C-e" (lambda ()
                                              (interactive)
                                              (viper-scroll-up-one 3)))
(define-key viper-vi-global-user-map ",f" 'find-file)
(define-key viper-vi-global-user-map ",r" 'lusty-file-explorer)
(define-key viper-vi-global-user-map ",b" 'lusty-buffer-explorer)
(define-key viper-vi-global-user-map (kbd "TAB")
                                     (lambda ()
                                       (interactive)
                                       (switch-to-buffer (other-buffer))))
(define-key viper-vi-global-user-map ",xo" 'other-window)
(define-key viper-vi-global-user-map ",x0" 'delete-window)
(define-key viper-vi-global-user-map ",x1" 'delete-other-windows)
(define-key viper-vi-global-user-map ",A" 'beginning-of-defun)
(define-key viper-vi-global-user-map ",v" 'vimpulse-vim-excursion)
(define-key viper-vi-global-user-map ",xl"
                                     'qpx-slime-startup-or-goto-existing-repl)
(define-key viper-vi-global-user-map ",xq" 'slime-quit-lisp)
(define-key viper-vi-global-user-map ",xs" 'slime-selector)
(define-key viper-vi-global-user-map "=" 'indent-according-to-mode)
(define-key viper-vi-global-user-map ";" 'execute-extended-command)

;; Add more viper-ified modes
(setq viper-vi-state-mode-list
      (append viper-vi-state-mode-list
              '(grep-mode
                slime-xref-mode
                slime-repl-mode
                sldb-mode
                help-mode
                Info-mode
                debugger-mode
                apropos-mode
                completion-list-mode)))

(setq viper-emacs-state-mode-list
      (set-difference viper-emacs-state-mode-list
                      '(Info-mode help-mode completion-list-mode)))


;; Help-mode fixes
(defvar viper-help-mode-fixes
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'View-quit)
    ;(define-key map [return] 'help-follow)
    map))
(viper-modify-major-mode 'help-mode 'vi-state viper-help-mode-fixes)

;; Info-mode fixes
(defvar viper-info-mode-fixes 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'Info-exit)
    (define-key map (kbd "RET") 'Info-follow-nearest-node)
    map))
(viper-modify-major-mode 'Info-mode 'vi-state viper-info-mode-fixes)

;; Apropos-mode fixes
(defvar viper-apropos-mode-fixes
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map (kbd "RET") 'apropos-follow)
    map))
(viper-modify-major-mode 'apropos-mode 'vi-state viper-apropos-mode-fixes)

;; Debugger-mode fixes
(defvar viper-debugger-mode-fixes 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'top-level)
    map))
(viper-modify-major-mode 'debugger-mode 'vi-state viper-debugger-mode-fixes)

;; SLIME Debugger fixes
(add-hook 'sldb-mode-hook 'viper-change-state-to-vi)

;; SLIME XREF fixes
(defvar viper-slime-xref-fixes 
  (let ((map (make-sparse-keymap)))
    ;; STEVE: vvv doesn't work vvv (or, works by coincidence)
    (define-key map (kbd "RET") 'slime-show-xref)
    ;; STEVE: vvv doesn't work vvv (overridden by viper)
    (define-key map (kbd "SPACE") 'slime-goto-xref)
    (define-key map "q" 'slime-xref-quit)
    map))
(viper-modify-major-mode 'slime-xref-mode 'vi-state viper-slime-xref-fixes)
(add-hook 'slime-xref-mode-hook 'viper-change-state-to-vi)

;; Grep mode fixes
(defvar viper-grep-mode-fixes 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'close-buffer-and-window-unless-last)
    map))
(viper-modify-major-mode 'grep-mode 'vi-state viper-grep-mode-fixes)

;; SLIME macroexpansion mode fixes
(defun steve-slime-temp-buffer-fixes ()
  (when (string-equal (buffer-name) "*SLIME macroexpansion*")
    (viper-change-state-to-vi)
    (viper-add-local-keys 'vi-state '(("q" . slime-temp-buffer-quit)))))
(add-hook 'lisp-mode-hook 'steve-slime-temp-buffer-fixes)

;; SLIME REPL fixes
(add-hook 'slime-repl-mode-hook 'viper-change-state-to-vi)


;TODO:
; - remember viper-harness-minor-mode
; map ESC to end visual mode (which C-g does)
; map ESC to end search mode (which C-g does)
; map C-[ to end search mode (which C-g does)
; remember "] register"
; - in repl, RETURN runs expr unless it's above prompt
; *grep*:
;  <SPACE>-r
; - map key to select *scratch*
; - try viper-syntax-preference set to extended