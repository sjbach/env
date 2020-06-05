;;; -*- lexical-binding: t; -*-

;;;
;;; Package loading.
;;;

;; This needs to be done before loading evil:
;; Enable evil in the minibuffer.
(customize-set-variable 'evil-want-minibuffer t)
;; Normally I'd want to enable this, but it's superceded by evil-collection.
;; Context: https://github.com/emacs-evil/evil-collection/issues/60
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-symbol-word-search t)
;; Make "Y" behave like "D" and "C" instead of "yy".
(customize-set-variable 'evil-want-Y-yank-to-eol t)
;;
;; I prefer 'evil-search but its performance is very poor on large buffers.
;; (customize-set-variable 'evil-search-module 'evil-search)
;; (evil-select-search-module 'evil-search-module 'isearch)
;;
(require 'evil)
(evil-mode 1)

;; Save and restore markers across sessions.
(add-to-list 'desktop-locals-to-save 'evil-markers-alist)

;; This evil-collection setting appears to pre-date `evil-want-minibuffer' in
;; the standard evil package, so I'm not clear whether or not it's redundant,
;; but I notice setting it makes the custom lusty-explorer bindings work
;; correctly (below).
(customize-set-variable 'evil-collection-setup-minibuffer t)
(require 'evil-collection)
;; Enable for all modes in the collection (until I decide that's a bad idea):
(evil-collection-init)

;; Magit
(setq evil-magit-state 'motion)
(require 'evil-magit)
;; Override remapping of ":", as I use it for `execute-extended-command'.
;; Remove mapping for `evil-ex'
(evil-define-key evil-magit-state magit-mode-map ":" nil)

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; quickscope
(require 'evil-quickscope)
(global-evil-quickscope-mode 1)

(require 'evil-numbers)

;;;
;;; Tweaks and overrides of default Evil bindings.
;;;

;; Evil mappings should already be loaded now; don't want to use
;; with-eval-after-load and confuse the control flow.
(cl-assert (featurep 'evil-maps))

;; The default Evil bindings for these keys aren't that useful whereas various
;; major-mode bindings on these keys often are useful. Drop the Evil bindings
;; so that the major-mode bindings come to the surface.
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-motion-state-map " " nil)

;; Switch the mappings for "'" and "`"; evil-goto-mark is better in almost
;; every case, so best that it's more accessible
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; Entering insert state wll clear the echo area but usually the echo area
;; shows context relevant to why I am entering insert state, e.g. an eldoc
;; annotation. Prevent a clear of the echo area when I enter insert state.
(setq evil-insert-state-message nil)  ; default: "-- INSERT --"
;;
(defvar steve--previous-message nil)
;;
(defun steve--save-current-message ()
  (setq steve--previous-message
        (current-message)))
;;
(defun steve--apply-saved-message-to-echo-area ()
  (when steve--previous-message
    (let ((message-log-max nil))  ; don't show in *Messages*
      (message "%s" steve--previous-message))))
;;
(add-hook 'echo-area-clear-hook  ; run before every command, I think
          #'steve--save-current-message)
(add-hook 'evil-insert-state-entry-hook
          #'steve--apply-saved-message-to-echo-area)
;; (remove-hook 'echo-area-clear-hook #'steve--save-current-message)
;; (remove-hook 'evil-insert-state-entry-hook #'steve--apply-saved-message-to-echo-area)

;; C-o is bound to evil-jump-backward, which is useful; in Evil/Vim the counter
;; direction is C-i (see: `evil-want-C-i-jump'), but in terminal Emacs that key
;; is indistinguishable from TAB by default. Punt on the implications of
;; "fixing" that and set this binding instead.
(define-key evil-motion-state-map (kbd "C-S-o") #'evil-jump-forward)
;; STEVE would like the jump list to be tied to a frame/workspace;
;; (setq evil-jumps-cross-buffers nil)  ;; default: t

;; I don't use :ex mode (evil-ex).
(define-key evil-motion-state-map ":" #'execute-extended-command)
;; `pp-eval-expression' is generally superior to `eval-expression'.
(define-key evil-motion-state-map "\M-:" #'pp-eval-expression)
;; S-M-: is awkward.
(define-key evil-motion-state-map "\M-;" #'pp-eval-expression)

(evil-define-key*
  'motion 'global
  ;; Instead of `evil-next-line-first-non-blank'.
  "+" 'evil-numbers/inc-at-pt
  ;; Instead of `evil-previous-line-first-non-blank'.
  "-" 'evil-numbers/dec-at-pt)

(evil-define-key*
  'normal 'global
  ;; I see these bindings as risky. In usual Emacs usage quitting is quite
  ;; rare. I never want it to happen willy-nilly. So unset them.
  "ZZ" nil
  "ZQ" nil

  ;; Instead of `pop-tag-mark'. I use different bindings for tag stuff.
  (kbd "C-t") nil

  ;; Instead of inheriting `undo' or `undo-tree-undo'.
  (kbd "C-/") 'steve-remove-evil-search-highlight

  ;; Use "s" to save the current buffer instead of substitute. For my usage
  ;; pattern the save operation happens about twice as frequently as
  ;; substitution.
  ;;
  ;; Replaces `evil-substitute'. (Restored to original binding in visual state.)
  "s" #'save-buffer
  ;; Replaces `evil-change-whole-line', which is functionally the same as "cc".
  "S" #'evil-substitute

  ;; More easily repeatable visual-line scrolling than gj / gk.
  (kbd "M-j") #'evil-next-visual-line      ; default: `default-indent-new-line'
  (kbd "M-k") #'evil-previous-visual-line  ; default: `kill-sentence'
  )

(evil-define-key*
  'visual 'global
  ;; Restoring the standard "s" binding in visual state, overridden above.
  ;; (Aside: "S" in visual state adopted by `evil-surround'.)
  "s" #'evil-substitute
  ;; "d" and "D" are redundant with "x" in visual state, so reassign them to
  ;; the `expand-region' package - much more useful.
  "d" 'er/expand-region
  "D" 'er/contract-region
  )

;; No accidental `evil-quit' call.
(define-key evil-window-map "q" nil)

(defun steve-eval-region-and-close-visual-mode (beg end)
  (interactive "r")
  (unless (region-active-p)
    (error "Region not active"))
  (eval-region beg end)
  (when (evil-visual-state-p)
    (setq deactivate-mark t)))

;; Remove /-style search highlights.
(defun steve-remove-evil-search-highlight ()
  (interactive)
  (evil-ex-delete-hl 'evil-ex-search))

; STEVE genericize: take a register char as a prefix arg
; - but: takes up entire `,e` namespace;
(defun steve-copy-register-unnamed-to-a ()
  (interactive)
  (evil-set-register ?a (evil-get-register ?\")))
(defun steve-copy-register-unnamed-to-b ()
  (interactive)
  (evil-set-register ?b (evil-get-register ?\")))
(defun steve-copy-register-unnamed-to-c ()
  (interactive)
  (evil-set-register ?c (evil-get-register ?\")))

;; In normal state I like ">" and "<" to operate immediately on the current
;; line, one key press, rather than wait for a motion. Delegate to the regular
;; operator for visual state.
(require 'evil-types)  ;; For `(interactive "<v><vc>")`
(evil-define-operator steve-evil-shift-left (beg end
                                             &optional
                                             dummy-type count preserve-empty)
  :type line
  (interactive "<v><vc>")
  (steve-evil-shift-right beg end dummy-type (- (or count 1)) preserve-empty))
;;
(evil-define-operator steve-evil-shift-right (beg end
                                              &optional
                                              dummy-type count preserve-empty)
  :type line
  (interactive "<v><vc>")
  (save-excursion  ; don't move point to first non-whitespace character
    (if (and beg end)
        (evil-shift-right beg end count preserve-empty)
      (evil-shift-right-line count))))
;;
;; Make "=" behave in normal state like it does in visual state, acting
;; immediately on the current rather than waiting for a motion.
(evil-define-operator steve-evil-indent (beg end &optional dummy)
  :move-point nil
  :type line
  (interactive "<v>")
  (if (and beg end)
      (evil-indent beg end)
    (evil-indent-line (line-beginning-position) (line-beginning-position))))
;; TODO: should I be using `evil-define-key*` for this instead?
(define-key evil-normal-state-map "<" #'steve-evil-shift-left)
(define-key evil-normal-state-map ">" #'steve-evil-shift-right)
(define-key evil-normal-state-map "=" #'steve-evil-indent)

;; Tweak the semantics of `$`
;;
;; In Evil (and Vim), a `$` in visual state will stretch the region to include
;; the trailing newline of the current line. But usually when I press `$` in
;; visual state I only want the region to extend to the character preceding the
;; newline. Sometimes I do want the newline though! So make it contextual. Two
;; presses will include the newline.
(evil-define-motion steve-evil-end-of-line (count)
  :type inclusive
  (if (and (null count)
           (evil-visual-state-p)
           (not (and (bolp) (eolp)))  ;; not on a blank line
           ;; Note: evil's visual end is not the same position as (point).
           (< (marker-position evil-visual-end) (line-end-position)))
      (progn
        (end-of-line)
        (evil-adjust-cursor))
    ;; Defer
    (evil-end-of-line count)))
(define-key evil-visual-state-map "$" #'steve-evil-end-of-line)

;; I like C-y and C-e to scroll faster.
;;
;; Implemented as commands (with `:repeat nil`) because otherwise these
;; interfere with repeat ("."). There's probably a better way to do this.
(evil-define-command steve-evil-scroll-line-down (count)
  "Scrolls the window 3 * COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-up (* 4 count))))
;;
(evil-define-command steve-evil-scroll-line-up (count)
  "Scrolls the window 3 * COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-down (* 4 count))))
;;
(evil-define-command steve-evil-scroll-line-down-other (count)
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-other-window (* 4 count)))
;;
(evil-define-command steve-evil-scroll-line-up-other (count)
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-other-window-down (* 4 count)))

(evil-define-key*
  'motion 'global
  ;; (Overriding evil-scroll-line-up, evil-scroll-line-down)
  "\C-y" #'steve-evil-scroll-line-up
  "\C-e" #'steve-evil-scroll-line-down
  "\M-y" #'steve-evil-scroll-line-up-other
  "\M-e" #'steve-evil-scroll-line-down-other)

(evil-define-key*
  ;; STEVE instead of global should be the map for fundamental-mode (if one
  ;; existed).
  'insert 'global
  ;; Scroll-other in insert mode.
  "\M-y" #'steve-evil-scroll-line-up-other     ;; overrides `keyboard-quit'
  "\M-e" #'steve-evil-scroll-line-down-other)  ;; overrides `forward-sentence'


;;;
;;; "," as a Vim-style leader key.
;;;

;; Note: overrides the traditional binding of ",", which is
;;       "Repeat latest f, t, F or T in opposite direction".
;;
(define-key evil-motion-state-map steve-dwim-leader-prefix-key steve-dwim-leader-map)

(evil-define-key*
  'motion grep-mode-map
  ;; (Instead of `recompile'.)
  "gg" 'evil-goto-first-line
  "q" 'kill-buffer-and-window)

;; See also: keys.el


;;;
;;; Overrides of default states in some modes.
;;;

(evil-set-initial-state 'completion-list-mode 'motion)


;;;
;;; Misc
;;;

;; Lusty Explorer
;; Have to do this in a hook because I wrote lusty-explorer.el in a weird way.
;; Note: only necessary because I've enabled `evil-want-minibuffer'.
(add-hook 'lusty-setup-hook
          (lambda ()
            (evil-define-key*
              '(insert normal motion global) lusty-mode-map
              (kbd "C-n") 'lusty-highlight-next
              (kbd "C-p") 'lusty-highlight-previous
              (kbd "C-s") 'lusty-highlight-next
              (kbd "C-r") 'lusty-highlight-previous)))

;; Vim-related utility functions.
;;

;; Note: could probably use `evil-edit` instead, but I'm used to this.
(defun e ()
  (interactive)
  (revert-buffer nil t))

(provide 'steve-evil-tweaks)

