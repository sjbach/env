;;; viper-in-more-modes.el --- vi-like keybindings for various Emacs modes

;; Copyright (C) 2007 Alessandro Piras, Brad Beveridge, Jason Spiro
;;
;; Author: Alessandro Piras <laynor at gmail.com>
;;      Brad Beveridge <brad.beveridge at gmail.com>
;;      Alexey Romanov <alexey.v.romanov at gmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>
;; Version: 0.1.3
;; URL: http://www.assembla.com/spaces/viper-in-more-modes/
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is an unofficial add-on to Michael Kifer's Viper-mode.
;; It provides vi-like keymaps for various major modes, including
;; emacs-lisp-mode, lisp-mode, lisp-interaction-mode, slime-repl-mode,
;; LaTeX-mode, haskell-mode, prolog-mode and ruby-mode. If you have
;; any questions or comments, please e-mail the authors and
;; maintainer. We provide no guarantee of help with such early code.
;; If you extend this file to cover additional modes, we would be very
;; grateful; please contact us.
;;
;; There are no installation instructions or usage instructions, but
;; we might be able to help you out if you contact us. If you wrote
;; such instructions and added them to EmacsWiki, we would appreciate
;; it!
;;
;; This is alpha-quality code. If it works for you, please let us
;; know.

;;; TODO:

;; * Rename viper-in-more-modes to something much shorter. This will
;; not only give viper-in-more-modes a shorter name, but will also
;; instantly provide us with a better prefix.
;;
;; * Clean up the code in general: for example, the error messages
;; shouldn't include exclamation marks. And the grammar and
;; capitalization in the comments should be improved. Also, the boxed
;; comments probably don't have to be in boxes.
;;
;; * Submit it to M-x report-emacs-bug and ask them to please include
;; viper-in-more-modes as part of Viper. If we can't reach some
;; contributors for copyright assignment, we'll probably have to
;; discard their contributions at this time, so we should probably try
;; to get all contributors' mailing addresses and phone numbers as
;; soon as they've contributed fifteen lines or more, in case they
;; later disappear.

;;; Change Log:

;; Version 0.1.3: Added bindings for Haskell, Ruby, Prolog and LaTeX
;; (from AUCTeX) modes. Thank you, Alexey Romanov <alexey.v.romanov at
;; gmail.com>. Changed prefix from "vimper" to "viper-imm" and renamed
;; `viper-leader-char' to `viper-imm-leader-char'. Created a
;; customization group.
;; Version 0.1.2: Removed some duplicate keybinding code. Also,
;; slime-list-callees is now on the ">" key instead of the "<" key,
;; which was already taken. Thank you, Stephen Bach <sjbach at
;; comcast.net>.
;; Version 0.1.1: Made `viper-leader-char' a var, not a const. Thank
;; you, John J Foerch <jjfoerch at earthlink.net>.
;; Version 0.1: Initial upload to EmacsWiki.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

;; Customization group
(defgroup viper-in-more-modes nil
  "vi-like keybindings for various modes."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'viper-imm)

;; Begin utility code {{{

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro workaround to make some commands  ;;;
;;; work on the character the cursor is on  ;;;
;;; too (e.g., in Visual mode, pressing "d" ;;;
;;; also deletes the char under the cursor  ;;;
;;; (like in Vim))                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STEVE FIXME: should use save-excursion instead?
(defmacro do-one-char-forward (&rest body)
  "Wraps the body between `forward-char' and `backward-char' to make commands
work on closed parens like one can expect in vi."
  `(progn
     (forward-char)
     ,@body
     (backward-char)))

(defmacro def-simple-viper-imm-wrapper-ocf (name args &rest body)
  "Define a wrapper for a command to execute it as if the cursor was one
   char forward the current position. Uses `do-one-char-forward'. Use it
   like a defun without lambda-list.  See examples below."
  `(defun ,name (,@args)
     (interactive)
     (do-one-char-forward
      ,@body)))

(defcustom viper-imm-leader-char " "
  "Leader char for viper-in-more-modes keymaps.")

(defmacro viper-imm-defkey-l (map key func)
  `(define-key ,map (concat viper-imm-leader-char ,key) ,func))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Emacs Lisp Mode - Viper Mappings       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom viper-imm-lisp-bindings t
  "Lisp bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

;; Commands definitions (almost all are workarounds due to the fact
;; that Emacs wants the cursor to be AFTER the ")" to execute
;; functions on S-expressions. We use the `do-one-char-forward'
;; utility macro here (see above for details on that macro).
(def-simple-viper-imm-wrapper-ocf viper-imm-eval-print-last-sexp ()
  (eval-print-last-sexp))

(def-simple-viper-imm-wrapper-ocf viper-imm-eval-last-sexp (&optional eval-last-sexp-arg-internal)
  (eval-last-sexp eval-last-sexp-arg-internal))

(defun viper-imm-eval-region (&optional arg)
  (interactive "P")
  (if (and (boundp 'vimpulse-visual-mode)
           (not vimpulse-visual-mode))
      (error "Select the region in Visual Mode.")
    (eval-region (min (mark) (point)) (max (mark) (point)))
    (vimpulse-visual-mode 'toggle)))

(defun viper-imm-pp-eval-region ()
  (interactive)
  (message (pp-to-string (viper-imm-eval-region))))

(def-simple-viper-imm-wrapper-ocf viper-imm-pp-eval-last-sexp (&optional eval-last-sexp-arg-internal)
  (pp-eval-last-sexp eval-last-sexp-arg-internal))

(def-simple-viper-imm-wrapper-ocf viper-imm-macroexpand ()
  (pp-macroexpand-expression (sexp-at-point)))
(def-simple-viper-imm-wrapper-ocf viper-imm-macroexpand-all ()
  (message (pp-to-string (macroexpand-all (sexp-at-point)))))

;; Bindings
(defvar viper-imm-emacs-lisp-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "pe" 'viper-imm-pp-eval-last-sexp)
    (viper-imm-defkey-l map "pE" 'pp-eval-expression)
    (viper-imm-defkey-l map "pr" 'viper-imm-pp-eval-region)
    (viper-imm-defkey-l map "e" 'viper-imm-eval-last-sexp)
    (viper-imm-defkey-l map "j" 'viper-imm-eval-print-last-sexp)
    (viper-imm-defkey-l map "r" 'viper-imm-eval-region)
    (viper-imm-defkey-l map "K" 'eval-buffer)
    (viper-imm-defkey-l map "da" 'apropos)
    (viper-imm-defkey-l map "df" 'describe-function)
    (viper-imm-defkey-l map "dv" 'describe-variable)
    (viper-imm-defkey-l map "E"  'eval-expression)
    (viper-imm-defkey-l map "m" 'viper-imm-macroexpand)
    (viper-imm-defkey-l map "M" 'viper-imm-macroexpand-all)
    (viper-imm-defkey-l map "B" 'byte-compile-file)
    map))

(when viper-imm-lisp-bindings
  (viper-modify-major-mode 'emacs-lisp-mode
                           'vi-state
                           viper-imm-emacs-lisp-mode-vi-map)
  (viper-modify-major-mode 'lisp-interaction-mode
                           'vi-state
                           viper-imm-emacs-lisp-mode-vi-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common Lisp Mode - Viper Mappings ;;;
;;;                Slime              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom viper-imm-slime-bindings t
  "Common Lisp Slime bindings."
  :type  'boolean
  :group 'viper-in-more-modes)

;; Commands
(def-simple-viper-imm-wrapper-ocf viper-imm-slime-compile-defun ()
   (slime-compile-defun))

(def-simple-viper-imm-wrapper-ocf viper-imm-slime-eval-defun ()
   (slime-eval-defun))

(def-simple-viper-imm-wrapper-ocf viper-imm-slime-eval-last-expression ()
   (slime-eval-last-expression))

(defun viper-imm-slime-eval-print-last-expression ()
  (interactive)
  (unless (eolp)
    (forward-char))
  (let ((string (slime-last-expression)))
    (insert "\n")
    (slime-eval-print string)))

(def-simple-viper-imm-wrapper-ocf viper-imm-slime-pprint-eval-last-expression ()
   (slime-pprint-eval-last-expression))

(defun viper-imm-slime-eval-region ()
  (interactive)
  (if (and (boundp 'vimpulse-visual-mode)
           (not vimpulse-visual-mode))
      (error "Select the region in Visual mode.")
    (slime-eval-region (min (mark) (point)) (max (mark) (point)))
    (vimpulse-visual-mode 'toggle)))

;; Bindings
;;
;; In general, the Viper Slime mappings are much the same as regular
;; Slime bindings. The C-c and C-x prefixes are dropped. When commands
;; are similar, we use a lower case letter for the C-<key> case and an
;; upper case letter for the M-<key>, such as:
;;
;; C-c C-k : slime-compile-and-load-file : (vip-slime-leader k)
;; C-c M-k : slime-compile-file          : (vip-slime-leader K)
;;
;; All commands begin with vip-slime-leader, which defaults to
;; <space>. The M-x commands are not mapped, as they are presumably
;; rare. Some keys are a triple key sequence. The second key is a
;; marker for a category the third key is the activation key.
(defvar viper-imm-lisp-mode-vi-map
  (let ((map (make-sparse-keymap)))

    ;; ITA
    (when (itap)
      (viper-imm-defkey-l map "g" 'qgrep)
      (viper-imm-defkey-l map "\C-i" 'insert-dp)
      (viper-imm-defkey-l map "\C-r" 'remove-dp))

    ;; Compilation Commands
    (viper-imm-defkey-l map "k" 'slime-compile-and-load-file)
    (viper-imm-defkey-l map "K" 'slime-compile-file)
    (viper-imm-defkey-l map "c" 'viper-imm-slime-compile-defun)
    (viper-imm-defkey-l map "C" 'slime-remove-notes)

    ;; Note handling has the same binding as Slime defaults
    (viper-imm-defkey-l map "M-n" 'slime-next-note)
    (viper-imm-defkey-l map "M-p" 'slime-previous-note)

    ;; Finding definitions (they are same as Slime default)
    (viper-imm-defkey-l map "." 'slime-edit-definition)   
    (viper-imm-defkey-l map "," 'slime-pop-find-definition-stack)

    ;; Lisp Evaluation
    (viper-imm-defkey-l map "x" 'viper-imm-slime-eval-defun)
    (viper-imm-defkey-l map "e" 'viper-imm-slime-eval-last-expression)
    (viper-imm-defkey-l map "j" 'viper-imm-slime-eval-print-last-expression)
;    (viper-imm-defkey-l map "p" 'viper-imm-slime-pprint-eval-last-expression)
    ; watch for visual mode!!
    (viper-imm-defkey-l map "r" 'viper-imm-slime-eval-region) 

    ;; Lisp Documentation
    ;; 3 key sequences
    (viper-imm-defkey-l map "dd" 'slime-describe-symbol) 
    (viper-imm-defkey-l map "da" 'slime-apropos) 
    (viper-imm-defkey-l map "dz" 'slime-apropos-all)
    (viper-imm-defkey-l map "dp" 'slime-apropos-package)
    (viper-imm-defkey-l map "dh" 'slime-hyperspec-lookup)
    (viper-imm-defkey-l map "d~" 'common-lisp-hyperspec-format)
    (viper-imm-defkey-l map "dG" 'slime-repl-clear-buffer)

    ;; Macro expansion
    (viper-imm-defkey-l map "m" 'slime-macroexpand-1)
    (viper-imm-defkey-l map "M" 'slime-macroexpand-all)
    (viper-imm-defkey-l map "t" 'slime-toggle-trace-fdefinition)

    ;; Disassembly
    (viper-imm-defkey-l map "D" 'slime-disassemble-symbol)

    ;; Abort/Recovery
    (viper-imm-defkey-l map "b" 'slime-interrupt)
    (viper-imm-defkey-l map "~" 'slime-sync-package-and-default-directory)
    (viper-imm-defkey-l map "P" 'slime-repl-set-package)
        
    ;; Cross-reference
    ;; 3 key sequences
    (viper-imm-defkey-l map "wc" 'slime-who-calls)
    (viper-imm-defkey-l map "wr" 'slime-who-references)
    (viper-imm-defkey-l map "wb" 'slime-who-binds)
    (viper-imm-defkey-l map "ws" 'slime-who-sets)
    (viper-imm-defkey-l map "wm" 'slime-who-macroexpands)
    (viper-imm-defkey-l map "<" 'slime-list-callers)
    (viper-imm-defkey-l map ">" 'slime-list-callees)

    ;; Inspector
    (viper-imm-defkey-l map "i" 'slime-inspect)

    ;; Repl!
    (viper-imm-defkey-l map "R" 'slime-switch-to-output-buffer)
    (viper-imm-defkey-l map "z" 'slime-switch-to-output-buffer)
    (viper-imm-defkey-l map "s" 'slime-scratch)
        
    ;; Profiler
    ;; "p" is already taken as a key, we
    ;; use "f" to access the profiler functions
    (viper-imm-defkey-l map "ft" 'slime-toggle-profile-fdefinition)
    (viper-imm-defkey-l map "fp" 'slime-profile-package)
    (viper-imm-defkey-l map "fu" 'slime-unprofile-all)
    (viper-imm-defkey-l map "fr" 'slime-profile-report)
    (viper-imm-defkey-l map "fR" 'slime-profile-reset)
    map))

(when viper-imm-slime-bindings 
  (viper-modify-major-mode 'lisp-mode
                           'vi-state
                           viper-imm-lisp-mode-vi-map)
  (viper-modify-major-mode 'clojure-mode
                           'vi-state
                           viper-imm-lisp-mode-vi-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Slime REPL Mode - Viper Mappings    ;;;
;;;                  Slime                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commands

(defun viper-imm-slime-repl-forward ()
  (interactive)
  (slime-repl-history-replace 'forward))

(defun viper-imm-slime-repl-backward ()
  (interactive)
  (slime-repl-history-replace 'backward))

;; Bindings

(defvar viper-imm-slime-repl-mode-vi-map
  (let ((map (copy-keymap viper-imm-lisp-mode-vi-map)))
    (viper-imm-defkey-l map "n" 'viper-imm-slime-repl-forward)
    (viper-imm-defkey-l map "p" 'viper-imm-slime-repl-backward)
    (define-key map "\C-n" 'viper-imm-slime-repl-forward)
    (define-key map "\C-p" 'viper-imm-slime-repl-backward)
    (define-key map (kbd "RET") 'slime-repl-closing-return)
    map))

(defvar viper-imm-slime-repl-mode-insert-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'slime-repl-closing-return)
    (define-key map "\C-n" 'viper-imm-slime-repl-forward)
    (define-key map "\C-p" 'viper-imm-slime-repl-backward)
    map))

(when viper-imm-slime-bindings 
  (viper-modify-major-mode 'slime-repl-mode
                           'vi-state
                           viper-imm-slime-repl-mode-vi-map)
  (viper-modify-major-mode 'slime-repl-mode
                           'insert-state
                           viper-imm-slime-repl-mode-insert-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime Debugger Mode

;; Bindings

(defvar viper-imm-sldb-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'sldb-default-action)
    (define-key map "q" 'sldb-quit)
    (viper-imm-defkey-l map "0" 'sldb-invoke-restart-0)
    (viper-imm-defkey-l map "1" 'sldb-invoke-restart-1)
    (viper-imm-defkey-l map "2" 'sldb-invoke-restart-2)
    (viper-imm-defkey-l map "3" 'sldb-invoke-restart-3)
    (viper-imm-defkey-l map "4" 'sldb-invoke-restart-4)
    (viper-imm-defkey-l map "5" 'sldb-invoke-restart-5)
    (viper-imm-defkey-l map "6" 'sldb-invoke-restart-6)
    (viper-imm-defkey-l map "7" 'sldb-invoke-restart-7)
    (viper-imm-defkey-l map "8" 'sldb-invoke-restart-8)
    (viper-imm-defkey-l map "9" 'sldb-invoke-restart-9)
    (viper-imm-defkey-l map "a" 'sldb-abort)
    (viper-imm-defkey-l map "c" 'sldb-continue)
    (viper-imm-defkey-l map "v" 'sldb-show-source)
    map))

(when viper-imm-slime-bindings 
  (viper-modify-major-mode 'sldb-mode
                           'vi-state
                           viper-imm-sldb-mode-vi-map))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++ Mode

;; Bindings

(defvar viper-imm-c-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (viper-imm-defkey-l map "." 'find-tag)
    (viper-imm-defkey-l map "," 'pop-tag-mark)
    (when (itap)
      (viper-imm-defkey-l map "g" 'qgrep))
    map))
(viper-modify-major-mode 'c-mode 'vi-state
                         viper-imm-c-mode-vi-map)
(viper-modify-major-mode 'c++-mode 'vi-state
                         viper-imm-c-mode-vi-map)

;; End major mode keybinding code

(provide 'viper-in-more-modes)

;;; viper-in-more-modes.el ends here
