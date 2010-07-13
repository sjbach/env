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
(setq-default viper-translate-all-ESC-keysequences nil)
(setq-default viper-change-notification-threshold 200)

(cond ((boundp 'viper-ESC-keyseq-timeout)
       (setq-default viper-ESC-keyseq-timeout 0))
      ((fboundp 'viper-ESC-keyseq-timeout)
       (setq-default viper-fast-keyseq-timeout 0)
       (defadvice viper-ESC-keyseq-timeout
           (around fix-viper-terminal-ESC activate)
         (setq ad-return-value 0)))
      (t
       (error "unknown ESC timeout method")))

; Allow backspace past start of edit and beginning of line.
(setq-default viper-ex-style-editing nil)  

; Non-sluggish paren matching (using "%" key).
(viper-set-parsing-style-toggling-macro 'undefine)

; Vimpulse/Viper modification
(define-key viper-insert-global-user-map "\C-g" 'viper-intercept-ESC-key)
(define-key viper-vi-global-user-map "\C-d"
                                     'close-buffer-and-window-unless-last)
(define-key viper-vi-global-user-map "\C-y" (lambda ()
                                              (interactive)
                                              (viper-scroll-down-one 3)))
(define-key viper-vi-global-user-map "\C-e" (lambda ()
                                              (interactive)
                                              (viper-scroll-up-one 3)))
(define-key viper-vi-global-user-map ",l" 'hs-toggle-hiding)
(define-key viper-vi-global-user-map ",f" 'find-file)
(define-key viper-vi-global-user-map ",r" 'lusty-file-explorer)
(define-key viper-vi-global-user-map ",b" 'lusty-buffer-explorer)
(define-key viper-vi-global-user-map (kbd "TAB")
                                     (lambda ()
                                       (interactive)
                                       (switch-to-buffer (other-buffer))))
(define-key viper-vi-global-user-map ",h" 'ff-find-other-file)
(define-key viper-vi-global-user-map ",xo" 'other-window)
(define-key viper-vi-global-user-map ",x0" 'delete-window)
(define-key viper-vi-global-user-map ",x1" 'delete-other-windows)
(define-key viper-vi-global-user-map ",x2" 'split-window-vertically)
(define-key viper-vi-global-user-map ",x3" 'split-window-horizontally)
(define-key viper-vi-global-user-map ",xk" 'kill-this-buffer)
(define-key viper-vi-global-user-map ",A" 'beginning-of-defun)

(defun vimpulse-vim-excursion ()
  ;; FIXME cleanup
  (interactive)
  (let ((file (buffer-file-name)))
    (cond ((null file) (message "Buffer not visiting a file"))
          ((buffer-modified-p) (message "Buffer is modified!"))
          (t
           (call-process "gvim" nil nil nil file)
           (ex-edit)))))

(define-key viper-vi-global-user-map ",v" 'vimpulse-vim-excursion)
(define-key viper-vi-global-user-map ",xl"
                                     'qpx-slime-startup-or-goto-existing-repl)
(define-key viper-vi-global-user-map ",xq" 'slime-quit-lisp)
(define-key viper-vi-global-user-map ",xs" 'slime-selector)
(define-key viper-vi-global-user-map "=" 'indent-according-to-mode)
(define-key viper-vi-global-user-map ":" 'execute-extended-command)
(define-key viper-vi-global-user-map ";" 'execute-extended-command)

;; Local modifications to viper-in-more-modes
;;
(eval-after-load "viper-in-more-modes"
  '(progn
    (when (itap)
      (viper-imm-defkey-l viper-imm-lisp-mode-vi-map "g" 'qgrep)
      (viper-imm-defkey-l viper-imm-lisp-mode-vi-map "\C-i" 'insert-dp)
      (viper-imm-defkey-l viper-imm-lisp-mode-vi-map "\C-r" 'remove-dp)
      (viper-imm-defkey-l viper-imm-slime-repl-mode-vi-map "g" 'qgrep)
      (viper-imm-defkey-l viper-imm-slime-repl-mode-vi-map "\C-i" 'insert-dp)
      (viper-imm-defkey-l viper-imm-slime-repl-mode-vi-map "\C-r" 'remove-dp))

    ;; Clojure inherits Lisp's bindings -- mostly correct.

    (when viper-imm-slime-bindings 
      (viper-modify-major-mode 'clojure-mode
                               'vi-state
                               viper-imm-lisp-mode-vi-map))

    ;;;; C/C++ Mode

    (defcustom viper-imm-c-bindings t
      "C/C++ bindings."
      :type  'boolean
      :group 'viper-in-more-modes)

    (defvar viper-imm-c-mode-vi-map
      (let ((map (make-sparse-keymap)))
        (viper-imm-defkey-l map "." 'find-tag)
        (viper-imm-defkey-l map "," 'pop-tag-mark)
        (when (itap)
          (viper-imm-defkey-l map "g" 'qgrep))
        map))

    (when viper-imm-c-bindings
      (viper-modify-major-mode 'c-mode
                               'vi-state
                               viper-imm-c-mode-vi-map)
      (viper-modify-major-mode 'c++-mode
                               'vi-state
                               viper-imm-c-mode-vi-map))))

;; Add more viper-ified modes
(setq viper-vi-state-mode-list
      (append viper-vi-state-mode-list
              '(fundamental-mode  ; << doesn't seem to work
                clojure-mode
                grep-mode
                comint-mode
                slime-xref-mode
                slime-repl-mode
                sldb-mode
                debugger-mode
                completion-list-mode)))

(setq viper-emacs-state-mode-list
      (set-difference viper-emacs-state-mode-list
                      '(completion-list-mode)))

;; Debugger-mode fixes
(defvar viper-debugger-mode-fixes 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'top-level)
    map))
(viper-modify-major-mode 'debugger-mode 'vi-state viper-debugger-mode-fixes)

;; SLIME Debugger fixes
(add-hook 'sldb-mode-hook 'viper-change-state-to-vi)

;; Grep mode fixes
(defvar viper-grep-mode-fixes 
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'close-buffer-and-window-unless-last)
    map))
(viper-modify-major-mode 'grep-mode 'vi-state viper-grep-mode-fixes)

;; SLIME macroexpansion mode -- force change to viper
(defun steve-slime-temp-buffer-fixes ()
  (when (string-equal (buffer-name) "*SLIME Macroexpansion*")
    (viper-change-state-to-vi)))
(add-hook 'lisp-mode-hook 'steve-slime-temp-buffer-fixes)
(add-hook 'clojure-mode-hook 'steve-slime-temp-buffer-fixes)

;; SLIME REPL fixes
(add-hook 'slime-repl-mode-hook 'viper-comint-mode-hook)

;; SLIME *slime-events* fix
(add-hook 'slime-connected-hook
          (lambda ()
            (save-excursion
              (set-buffer (slime-events-buffer))
              (viper-change-state-to-vi))))

;TODO:
; map ESC to end search mode (which C-g does)
; map C-[ to end search mode (which C-g does)
; - remember viper-harness-minor-mode
; - C-j combines lines and also removes spacing when merged line begins with )
; remember "] register"
