;;; viper-in-more-modes.el --- vi-like keybindings for various Emacs modes

;; Copyright (C) 2007 Alessandro Piras, Brad Beveridge, Jason Spiro
;; 
;; Version: x.x.x
;; Authors: Alessandro Piras <laynor@gmail.com>,
;;          Brad Beveridge <brad.beveridge@gmail.com>
;;          Stephen Bach <this-file@sjbach.com>
;; Maintainer: Stephen Bach
;; URL: http://emacswiki.org/elisp/viper-in-more-modes.el
;; 
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Note: This file has been heavily modified by Stephen Bach.
;;
;; This file is an unofficial add-on to viper-mode.  It currently
;; provides vi-like keymaps for emacs-lisp-mode, lisp-interaction-mode,
;; slime-repl-mode, and lisp-mode.  If you have any questions or
;; comments, please email the authors and maintainer.  We provide no
;; guarantee of help with such early code.  If you extended this file to
;; cover additional modes, we would be very grateful; please contact us.
;; 
;; There are no installation instructions or usage instructions, but
;; we might be able to help you out if you contact us.  If you wrote
;; such instructions and added them to this wiki page, we would be very
;; appreciative.
;; 
;; This is alpha-quality code.  If it works for you, we would
;; appreciate it if you let us know.

;;; License:
;; 
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Contributors:
;;
;; John J Foerch
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro workaround to make some commands ;;;
;;; work on the character the cursor is on ;;;
;;; too (eg. in visual mode pressing "d"   ;;;
;;; deletes also the char under the cursor ;;;
;;; (like in vim)                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STEVE FIXME: should use save-excursion instead?
(defmacro do-one-char-forward (&rest body)
  "Wraps the body between `forward-char' and `backward-char' to make commands
work on closed parens like one can expect in vi."
  `(progn
     (forward-char)
     ,@body
     (backward-char)))

(defmacro def-ocf (name args &rest body)
  "Define a wrapper for a command to execute it as if the cursor was one
   char forward the current position. Uses `do-one-char-forward'. Use it
   like a defun without lambda-list.  See examples below."
  `(defun ,name (,@args)
     (interactive)
     (do-one-char-forward
      ,@body)))

(defvar vimper-leader-char " ")
(defmacro vimper-define-key (map key func)
  "Define a key binding prefixed with `vimper-leader-char'."
  `(define-key ,map (concat vimper-leader-char ,key) ,func))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Emacs Lisp Mode - Viper Mappings       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commands definitions.  Almost all are work-arounds due to the fact that
;;; Emacs wants the cursor to be _after_ the ")" to execute functions
;;; on sexps. We use the `do-one-char-forward' utility macro here (see above).

(def-ocf vimper-eval-print-last-sexp ()
  (eval-print-last-sexp))

(def-ocf vimper-eval-last-sexp (&optional eval-last-sexp-arg-internal)
  (eval-last-sexp eval-last-sexp-arg-internal))

(defun vimper-eval-region (&optional arg)
  (interactive "P")
  (if (not viper-visual-mode)
      (message "Select the region in Visual Mode!")
    (eval-region (min (mark) (point)) (max (mark) (point)))
    (viper-visual-mode 'toggle)))

(defun vimper-pp-eval-region ()
  (interactive)
  (message (pp-to-string (vimper-eval-region))))

(def-ocf vimper-pp-eval-last-sexp (&optional eval-last-sexp-arg-internal)
  (pp-eval-last-sexp eval-last-sexp-arg-internal))

(def-ocf vimper-macroexpand ()
  (pp-macroexpand-expression (sexp-at-point)))
(def-ocf vimper-macroexpand-all ()
  (message (pp-to-string (macroexpand-all (sexp-at-point)))))

;;; Bindings

(defvar vimper-emacs-lisp-mode-vi-map
  (let ((map (make-sparse-keymap)))
    ;(vimper-define-key map "p" (make-sparse-keymap))
    (vimper-define-key map "pe" 'vimper-pp-eval-last-sexp)
    (vimper-define-key map "pE" 'pp-eval-expression)
    (vimper-define-key map "pr" 'vimper-pp-eval-region)
    (vimper-define-key map "e" 'vimper-eval-last-sexp)
    (vimper-define-key map "j" 'vimper-eval-print-last-sexp)
    (vimper-define-key map "r" 'vimper-eval-region)
    (vimper-define-key map "K" 'eval-buffer)
    (vimper-define-key map "da" 'apropos)
    (vimper-define-key map "df" 'describe-function)
    (vimper-define-key map "dv" 'describe-variable)
    (vimper-define-key map "E"  'eval-expression)
    (vimper-define-key map "m" 'vimper-macroexpand)
    (vimper-define-key map "M" 'vimper-macroexpand-all)
    (vimper-define-key map "B" 'byte-compile-file)
    map))

(viper-modify-major-mode 'emacs-lisp-mode 'vi-state
                         vimper-emacs-lisp-mode-vi-map)
(viper-modify-major-mode 'lisp-interaction-mode 'vi-state
                         vimper-emacs-lisp-mode-vi-map)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Common Lisp Mode - Viper Mappings      ;;;
;;;                    Slime                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commands
(def-ocf vimper-slime-compile-defun ()
   (slime-compile-defun))

(def-ocf vimper-slime-eval-defun ()
   (slime-eval-defun))

(def-ocf vimper-slime-eval-last-expression ()
   (slime-eval-last-expression))

(defun vimper-slime-eval-print-last-expression ()
  (interactive)
  (unless (eolp)
    (forward-char))
  (let ((string (slime-last-expression)))
    (insert "\n")
    (slime-eval-print string)))

(def-ocf vimper-slime-pprint-eval-last-expression ()
   (slime-pprint-eval-last-expression))

(defun vimper-slime-eval-region ()
  (interactive)
  (if (not viper-visual-mode)
      (message "Select the region in Visual Mode!")
    (slime-eval-region (min (mark) (point)) (max (mark) (point)))
    (viper-visual-mode 'toggle)))

;; Bindings

;; In general the Viper Slime mappings are much the same as regular Slime
;; bindings, with the C-c and C-x prefixes dropped.  When commands are
;; similar, we use a lower case letter for the C-<key> case and an upper case
;; letter for the M-<key>, such as:
;;
;;   slime-compile-and-load-file : C-c C-k -> (vimper-leader-char k)
;;   slime-compile-file          : C-c M-k -> (vimper-leader-char K)
;;
;; All commands begin with vimper-leader-char, which defaults to <space>.  The
;; M-x commands are not mapped, as they are presumably rare.  Some keys are a
;; triple key sequence.  The second key is a marker for a category, the third
;; key is the activation key.

(defvar vimper-lisp-mode-vi-map
  (let ((map (make-sparse-keymap)))

    ;; ITA
    (when (itap)
      (vimper-define-key map "g" 'qgrep)
      (vimper-define-key map "\C-i" 'insert-dp)
      (vimper-define-key map "\C-r" 'remove-dp))

    ;; Compilation Commands
    (vimper-define-key map "k" 'slime-compile-and-load-file)
    (vimper-define-key map "K" 'slime-compile-file)
    (vimper-define-key map "c" 'vimper-slime-compile-defun)
    (vimper-define-key map "C" 'slime-remove-notes)

    ;; Note handling has the same binding as Slime defaults
    (vimper-define-key map "M-n" 'slime-next-note)
    (vimper-define-key map "M-p" 'slime-previous-note)

    ;; Finding definitions (they are same as Slime default)
    (vimper-define-key map "." 'slime-edit-definition)   
    (vimper-define-key map "," 'slime-pop-find-definition-stack)

    ;; Lisp Evaluation
    (vimper-define-key map "x" 'vimper-slime-eval-defun)
    (vimper-define-key map "e" 'vimper-slime-eval-last-expression)
    (vimper-define-key map "j" 'vimper-slime-eval-print-last-expression)
;    (vimper-define-key map "p" 'vimper-slime-pprint-eval-last-expression)
    ; watch for visual mode!!
    (vimper-define-key map "r" 'vimper-slime-eval-region) 

    ;; Lisp Documentation
    ;; 3 key sequences
    (vimper-define-key map "dd" 'slime-describe-symbol) 
    (vimper-define-key map "da" 'slime-apropos) 
    (vimper-define-key map "dz" 'slime-apropos-all)
    (vimper-define-key map "dp" 'slime-apropos-package)
    (vimper-define-key map "dh" 'slime-hyperspec-lookup)
    (vimper-define-key map "d~" 'common-lisp-hyperspec-format)
    (vimper-define-key map "dG" 'slime-repl-clear-buffer)

    ;; Macro expansion
    (vimper-define-key map "m" 'slime-macroexpand-1)
    (vimper-define-key map "M" 'slime-macroexpand-all)
    (vimper-define-key map "t" 'slime-toggle-trace-fdefinition)

    ;; Disassembly
    (vimper-define-key map "D" 'slime-disassemble-symbol)

    ;; Abort/Recovery
    (vimper-define-key map "b" 'slime-interrupt)
    (vimper-define-key map "~" 'slime-sync-package-and-default-directory)
    (vimper-define-key map "P" 'slime-repl-set-package)
        
    ;; Cross-reference
    ;; 3 key sequences
    (vimper-define-key map "wc" 'slime-who-calls)
    (vimper-define-key map "wr" 'slime-who-references)
    (vimper-define-key map "wb" 'slime-who-binds)
    (vimper-define-key map "ws" 'slime-who-sets)
    (vimper-define-key map "wm" 'slime-who-macroexpands)
    (vimper-define-key map "<" 'slime-list-callers)
    (vimper-define-key map ">" 'slime-list-callees)

    ;; Inspector
    (vimper-define-key map "i" 'slime-inspect)

    ;; Repl!
    (vimper-define-key map "R" 'slime-switch-to-output-buffer)
    (vimper-define-key map "z" 'slime-switch-to-output-buffer)
    (vimper-define-key map "s" 'slime-scratch)
        
    ;; Profiler
    ;; "p" is already taken as a key, we
    ;; use "f" to access the profiler functions
;    (vimper-define-key map "f" (make-sparse-keymap))
    (vimper-define-key map "ft" 'slime-toggle-profile-fdefinition)
    (vimper-define-key map "fp" 'slime-profile-package)
    (vimper-define-key map "fu" 'slime-unprofile-all)
    (vimper-define-key map "fr" 'slime-profile-report)
    (vimper-define-key map "fR" 'slime-profile-reset)
    map))

(viper-modify-major-mode 'lisp-mode 'vi-state vimper-lisp-mode-vi-map)
(viper-modify-major-mode 'clojure-mode 'vi-state vimper-lisp-mode-vi-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Slime   REPL    Mode - Viper Mappings    ;;;
;;;                    Slime                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Commands

(defun vimper-slime-repl-forward ()
  (interactive)
  (slime-repl-history-replace 'forward))

(defun vimper-slime-repl-backward ()
  (interactive)
  (slime-repl-history-replace 'backward))

;; Bindings

(defvar vimper-slime-repl-mode-vi-map
  (let ((map (copy-keymap vimper-lisp-mode-vi-map)))
    (vimper-define-key map "n" 'vimper-slime-repl-forward)
    (vimper-define-key map "p" 'vimper-slime-repl-backward)
    (define-key map "\C-n" 'vimper-slime-repl-forward)
    (define-key map "\C-p" 'vimper-slime-repl-backward)
    (define-key map (kbd "RET") 'slime-repl-closing-return)
    map))
(viper-modify-major-mode 'slime-repl-mode 'vi-state
                         vimper-slime-repl-mode-vi-map)

(defvar vimper-slime-repl-mode-insert-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'slime-repl-closing-return)
    (define-key map "\C-n" 'vimper-slime-repl-forward)
    (define-key map "\C-p" 'vimper-slime-repl-backward)
    map))
(viper-modify-major-mode 'slime-repl-mode 'insert-state
                         vimper-slime-repl-mode-insert-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime Debugger Mode

;; Bindings

(defvar vimper-sldb-mode-vi-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'sldb-default-action)
    (define-key map "q" 'sldb-quit)
    (vimper-define-key map "0" 'sldb-invoke-restart-0)
    (vimper-define-key map "1" 'sldb-invoke-restart-1)
    (vimper-define-key map "2" 'sldb-invoke-restart-2)
    (vimper-define-key map "3" 'sldb-invoke-restart-3)
    (vimper-define-key map "4" 'sldb-invoke-restart-4)
    (vimper-define-key map "5" 'sldb-invoke-restart-5)
    (vimper-define-key map "6" 'sldb-invoke-restart-6)
    (vimper-define-key map "7" 'sldb-invoke-restart-7)
    (vimper-define-key map "8" 'sldb-invoke-restart-8)
    (vimper-define-key map "9" 'sldb-invoke-restart-9)
    (vimper-define-key map "a" 'sldb-abort)
    (vimper-define-key map "c" 'sldb-continue)
    (vimper-define-key map "v" 'sldb-show-source)
    map))
(viper-modify-major-mode 'sldb-mode 'vi-state
                         vimper-sldb-mode-vi-map)


;; End major mode keybinding code

(provide 'viper-in-more-modes)
