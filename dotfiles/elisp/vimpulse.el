;;; vimpulse.el --- emulates Vim's most useful features, including Visual mode



;; Copyright (C) 2007 Alessandro Piras and Brad Beveridge
;; 
;; Version: xxxxxx
;; Keywords: emulations
;; Human-Keywords: vim, visual-mode, rsi, ergonomics, Emacs pinky finger
;; Authors: Alessandro Piras <laynor@gmail.com>,
;;          Brad Beveridge <brad.beveridge@gmail.com>
;;          Stephen Bach <this-file@sjbach.com>
;; Maintainer: Jason Spiro <jasonspiro3@gmail.com>
;; License: GPLv2 or later, as described below under "License"
;; Compatibility: Works well with GNU Emacs 21.4 and 22.0.
;;                Causes problems with undo, but has no other problems, 
;;                on XEmacs 21.4.19.
;;                Please send us compatibility info re. other Emacsen.
;; URL: http://emacswiki.org/elisp/vimpulse.el
;; 
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Vimpulse emulates Vim's most useful features, including Visual
;; mode.  Vimpulse is a set of modifications to viper, the minor mode
;; that emulates Vi.  Vimpulse is not a minor mode; as soon as it is
;; loaded, viper will start working in a more Vim-like way.
;; 
;; Vimpulse is beta software.  It seems to work quite well already
;; though.  Patches and feature requests welcome.

;; You can use C-v to go into block visual mode, C-y to yank the
;; rectangular selection, and C-p to paste the yanked rectangle of
;; text.  C-y and C-p are used because it was a simple way to add
;; visual block mode in a way *close* to Vim without having to hack
;; viper mode to use the normal 'y' and 'p' keys.  (If you map 'y' and
;; 'p' instead, it screws up viper's "normal" yanking and pasting
;; [non-visual mode]).  In the future, it would be nice to see
;; vimpulse provide this the "right" way, but at this point I'm too
;; inexperienced with elisp to make that happen.
;;
;; Note that this "implementation" of visual block mode doesn't 
;; support yanking text to a specific register (i.e. "x C-y to yank
;; the selected text to register 'x').  But it appears (at this 
;; point) that none of the visual modes provided by vimpulse provide 
;; that capability (yet). 
;;
;; - (class of bugs) In visual or line visual mode, pressing things
;;   like C-g or C-SPC tends to confuse Vimpulse and do weird things.

;; - make sentence movement work like in Vim.  I wonder if this can be
;;   done by setting viper options.
;;     - In Vim, according to :help sentence, end of sentence is:
;;         - '.', '?', or '!'
;;         - then (optionally) one or more '"', ''', ')', and ']'
;;           characters
;;         - then a newline, space, or tab.
;;         - A paragraph or section boundary is also a sentence
;;           boundary, but I bet viper handles that, and if it doesn't,
;;           it should.
;;             - A paragraph begins after each truly empty line (no
;;               whitespace chars on it) or after certain col-1 nroff
;;               macros.  A sentence begins after a form feed (^L), or
;;               certain nroff macros, in column 1.
;;             - The characters '{' and '}' sometimes affect paragraph
;;               definitions.  See :help paragraph.
;;     - In Viper, on the other hand, I bet sentences are like in vi,
;;       where Tabs aren't whitespace, and you need at least two spaces
;;       after the punctuation mark.
;; 
;; - In vimpulse, like in real vim, C-r only does redo in command
;;   mode; in insert mode it does something else.  (In vimpulse that
;;   "something else" is reverse i-search.)  Should it do reverse
;;   i-search in insert mode too?
;; 
;; - When you press "v" for visual mode, Vimpulse modifies the mode
;;   section of the modeline, so it reads e.g. "(Emacs-Lisp visual)".
;;   Shouldn't it do something to the <V> indicator instead?
;; 
;; - In v / V mode, Vim makes sure there is always at least 1 char /
;;   line selected.  IMO it provides nice feedback as to whether
;;   visual mode is on or not.  Is this worth implementing?
;; 
;; - should gj and gk do longlines-style movement like in Vim?  I
;;   really must resolve my Windows vs. Unix line-length hangups by
;;   Googling or asking before I even think about this.

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


;; Define a helper function that sets up the viper keys in a given map.
;; This function is useful for creating movement maps or altering existing
;; maps
;(defun vimpulse-set-movement-keys-for-map (map)
;  ; STEVE: I don't use these.
;  ;(define-key map "\C-d" 'viper-scroll-up)
;  ;(define-key map "\C-u" 'viper-scroll-down)
;  (define-key map "\C-f" 'viper-scroll-screen)
;  (define-key map "\C-b" 'viper-scroll-screen-back)
;  (define-key map "j" 'viper-next-line)
;  (define-key map "k" 'viper-previous-line)
;  (define-key map "l" 'viper-forward-char)
;  (define-key map "h" 'viper-backward-char))
;;
;; EXAMPLE, the following lines enable Vim style movement in help
;; and dired modes.
;; create a movement map and set the keys
;(setq vimpulse-movement-map (make-sparse-keymap))
;(vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;(viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map) 
;(viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)

;;; Code:


(require 'advice)
;; Load redo.el if available.  Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21 doesn't
;; ship with APEL included.
(unless (featurep 'redo)
  (load "redo" 'noerror))

(defun w (&optional args)
  (interactive "p")
  (save-buffer args))

(defun q (&optional args)
  (interactive "P")
  (save-buffers-kill-emacs args))

(defun wq (&optional args)
  (interactive "P")
  (save-buffers-kill-emacs args))

(define-key viper-vi-global-user-map ":"    'execute-extended-command)
(define-key viper-vi-global-user-map "gf"   'find-file-at-point)
(define-key viper-vi-global-user-map "gg"   'vimpulse-goto-first-line) 
(define-key viper-vi-global-user-map "zb"   'viper-line-to-bottom)
(define-key viper-vi-global-user-map "zh"   'scroll-right)
(define-key viper-vi-global-user-map "zl"   'scroll-left)
(define-key viper-vi-global-user-map "zt"   'viper-line-to-top)
(define-key viper-vi-global-user-map "zz"   'viper-line-to-middle)
(define-key viper-vi-global-user-map "*"    'vimpulse-search-forward-for-symbol-at-point) 
(define-key viper-vi-global-user-map "#"    'vimpulse-search-backward-for-symbol-at-point) 
(define-key viper-vi-global-user-map " "    nil)
(define-key viper-vi-global-user-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-global-user-map "\C-t" 'pop-tag-mark)

; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

; Block Visual Mode keys
; STEVE: this stuff is buggy.
;(define-key viper-vi-global-user-map "\C-y" 'rm-kill-ring-save)
;(define-key viper-vi-global-user-map "\C-v" 'rm-set-mark)
;(define-key viper-vi-global-user-map "\C-p" 'yank-rectangle)

(defun vimpulse-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1)) 

(defun vimpulse-search-for-symbol-at-point (forward-p)
  "Search forwards or backwards for the symbol under point."
  (let* ((str (regexp-quote (thing-at-point 'symbol)))
         (search-str (concat "\\<" str "\\>"
                             "\\|"
                             "\\_<" str "\\_>")))
    (setq viper-s-string search-str)
    (setq viper-s-forward forward-p)
    (viper-search search-str forward-p 1)))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point nil))

(defun vimpulse-jump-to-tag-at-point ()
 (interactive)
 (let ((tag (thing-at-point 'word)))
   (find-tag tag)))

(defun vimpulse-vim-excursion ()
  ;; FIXME cleanup
  (interactive)
  (let ((file (buffer-file-name)))
    (cond ((null file) (message "Buffer not visiting a file"))
          ((buffer-modified-p) (message "Buffer is modified!"))
          (t
           (call-process "gvim" nil nil nil file)
           (ex-edit)))))

;; This function replaces viper's original viper-exec-change function
;; which is invoked by key sequences starting with 'c'.  When the user
;; requests a command like 'cw', this function calls a sequence like
;; 'dwi' instead.  This stops viper from indicating the change
;; operation with distracting colored overlays and $ signs.  Instead,
;; it simply deletes the text then enters Insert mode, like Vim does.
(defun viper-exec-change (m-com com)
  (save-excursion
    (viper-exec-delete m-com com))                            
  (viper-insert nil))

;; Viper is overreaching by caring whether a visited file is under version
;; control -- disable this check.
(defun viper-maybe-checkout (buf)
  nil)

;;
;; Vim-like paren (and bracket, curly-brace, etc.) matching
;;
(defadvice show-paren-function (around vimpulse-fix-show-paren activate)
  "Modifies paren matching under viper to work like Vim."
  (if viper-vi-basic-minor-mode
      (cond
       ((eq (syntax-class (syntax-after (point))) 5)
        ;; Character under cursor is a closing paren.
        (save-excursion
          (forward-char)  ;; TODO: need to watch out for end-of-buffer?
          ad-do-it))  
       ((eq (syntax-class (syntax-after (1- (point)))) 5)
        ;; Character before cursor is a closing paren -- show-paren-function
        ;; would match incorrectly if called.
        nil)
       (t ad-do-it))
    ad-do-it))

(provide 'vimpulse)


;; Begin visual mode code

(eval-when-compile (require 'easy-mmode))

;; local variables
(defgroup vimpulse-visual nil
  "visual-mode for viper"
  :prefix "vimpulse-visual-"
  :group 'emulations)

 (define-minor-mode vimpulse-visual-mode
  "Toggles visual mode in viper"
  :lighter " visual"
  :initial-value nil
  :global nil
  :group 'vimpulse-visual)   
(defvar vimpulse-visual-mode-map (make-sparse-keymap)
  "Viper Visual mode keymap. This keymap is active when viper is in VISUAL mode")
(defvar vimpulse-visual-mode-linewise nil
  "If non nil visual mode will operate linewise")
(defcustom vimpulse-visual-load-hook nil
  "Hooks to run after loading vimpulse-visual-mode."
  :type 'hook
  :group 'vimpulse-visual)

(defadvice viper-move-marker-locally (around vimpulse-move-marker-locally-wrap
                                      activate)
 (unless vimpulse-visual-mode
   ad-do-it))

(defadvice viper-deactivate-mark (around vimpulse-deactivate-mark-wrap
                                  activate)
 (unless vimpulse-visual-mode
   ad-do-it))

;; Let ESC disable visual mode.
(defadvice viper-intercept-ESC-key (around vimpulse-visual-mode-ESC activate)
  (if vimpulse-visual-mode
    (vimpulse-visual-mode 'toggle)
    ad-do-it))

(defmacro my-get-emulation-keymap ()
  (if (>= emacs-major-version 22)
      'viper--key-maps
      'minor-mode-map-alist))

(defadvice viper-normalize-minor-mode-map-alist (after vimpulse-add-visual-maps
                                                 activate)
  "This function modifies minor-mode-map-alists to include the visual mode keymap"
    (push (cons 'vimpulse-visual-mode vimpulse-visual-mode-map)
          (my-get-emulation-keymap)))

;; Keys that differ from normal mode
(defun vimpulse-visual-yank-command ()
  (interactive)
  (forward-char)
  (vimpulse-visual-mode 'toggle)
  (viper-prefix-arg-com ?r 1 ?y))

(defun vimpulse-visual-delete-command ()
  (interactive)
  (forward-char)
  (vimpulse-visual-mode 'toggle)
  (viper-prefix-arg-com ?r 1 ?d))

(defun vimpulse-visual-change-command ()
  (interactive)
  (forward-char)
  (vimpulse-visual-mode 'toggle)
  (viper-prefix-arg-com ?r 1 ?c))
 
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive "P")
  (forward-char)
  (vimpulse-visual-mode 'toggle)
  (cond
   ((= (mark) (point)) nil)
   (t 
    (if (< (mark) (point)) (exchange-point-and-mark))
    (viper-replace-char arg)
    (let ((c (char-after (point))))
      (dotimes (i (- (mark) (point)))
	(cond
	 ((member (char-after (point)) '(?\r ?\n))
	  (forward-char))
	  (t (delete-char 1)
	     (insert c))))))))

(define-key vimpulse-visual-mode-map "v" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "V" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "\C-v" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "x" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "D" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "y" 'vimpulse-visual-yank-command)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "C" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "s" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "S" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-mode-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-mode-map "O" 'exchange-point-and-mark)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-mode-map "." 'undefined)
;; Keys remaining to be implemented (TODO)
(define-key vimpulse-visual-mode-map "i" 'undefined)
(define-key vimpulse-visual-mode-map "u" 'undefined)
(define-key vimpulse-visual-mode-map "U" 'undefined)
(define-key vimpulse-visual-mode-map "t" 'undefined)
(define-key vimpulse-visual-mode-map "T" 'undefined)

(add-hook 'post-command-hook
          '(lambda ()
             (if (and vimpulse-visual-mode vimpulse-visual-mode-linewise)
                 (beginning-of-line))))


;;;###auto-load
(defun vimpulse-visual-mode-toggle (&optional arg)
  (interactive "P")
  (make-local-variable 'vimpulse-visual-mode-linewise)
  (unless vimpulse-visual-mode
    (deactivate-mark)
    (viper-change-state-to-vi))
  (when vimpulse-visual-mode
    (setq vimpulse-visual-mode-linewise nil)
    (set-mark (point))
    ;;(setq vimpulse-visual-linewise line-wise)
    ;;(viper-change-state 'VISUAL)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Force transient-mark-mode to have visual selection ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (fboundp 'transient-mark-mode)
	(transient-mark-mode t))))

(defun vimpulse-visual-mode-linewise (&optional arg)
  "Starts viper visual mode in `linewise' mode"
  (interactive "P")
  (beginning-of-line)
  (vimpulse-visual-mode 'toggle)
  (setq vimpulse-visual-mode-linewise t))
(add-hook 'vimpulse-visual-mode-hook 'vimpulse-visual-mode-toggle t)
(run-hooks 'vimpulse-visual-load-hook)

;; We need to detect when a command has deactivated the mark so that
;; Vimpulse is able to exit Visual mode
(defun vimpulse-detect-mark-deactivate ()
  (when (and vimpulse-visual-mode (not mark-active))
    (vimpulse-visual-mode 'toggle)))
(add-hook 'deactivate-mark-hook 'vimpulse-detect-mark-deactivate)

(define-key viper-vi-basic-map "v" 'vimpulse-visual-mode)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-mode-linewise)

(provide 'vimpulse-visual-mode)
