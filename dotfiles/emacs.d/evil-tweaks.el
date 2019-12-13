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
;;
;(setq-default evil-symbol-word-search t)
(customize-set-variable 'evil-symbol-word-search t)
;;
(require 'evil)
(evil-mode 1)

;; Save and restore markers across sessions.
(add-to-list 'desktop-locals-to-save 'evil-markers-alist)

;; This evil-collection setting appears to pre-date `evil-want-minibuffer` in
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
;; Override remapping of ":", as I use it for `execute-extended-command`.
;; Remove mapping for `evil-ex`
(evil-define-key evil-magit-state magit-mode-map ":" nil)
;; Remove mapping for `magit-git-command`
(define-key magit-status-mode-map ":" nil)
(evil-define-key evil-magit-state magit-mode-map (kbd "C-SPC 1")
  #'magit-section-show-level-1-all)
(evil-define-key evil-magit-state magit-mode-map (kbd "C-SPC 2")
  #'magit-section-show-level-2-all)
(evil-define-key evil-magit-state magit-mode-map (kbd "C-SPC 3")
  #'magit-section-show-level-3-all)
(evil-define-key evil-magit-state magit-mode-map (kbd "C-SPC 4")
  #'magit-section-show-level-4-all)

;; Surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;;;
;;; Overrides of default evil bindings.
;;;

;; Evil mappings should already be loaded now; don't want to use
;; with-eval-after-load and confuse the control flow.
(cl-assert (featurep 'evil-maps))

;; The default Evil bindings for these keys aren't that useful while the
;; default major-mode bindings often are useful. Drop the Evil bindings so that
;; the major-mode bindings are made available.
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-motion-state-map " " nil)

;; Switch the mappings for "'" and "`"; evil-goto-mark is better in almost
;; every case, so best that it's more accessible
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; I don't use :ex mode (evil-ex).
(define-key evil-motion-state-map ":" #'execute-extended-command)
;; `pp-eval-expression' is generally superior to `eval-expression'.
(define-key evil-motion-state-map "\M-:" #'pp-eval-expression)
;; S-M-: is awkward.
(define-key evil-motion-state-map "\M-;" #'pp-eval-expression)

(evil-define-key*
  'normal 'global
  ;; I see these bindings as risky. In usual Emacs usage, quitting is quite
  ;; rare. I never want it to happen willy-nilly. So unset them.
  "ZZ" nil
  "ZQ" nil
  ;; I use different bindings for tag stuff.
  "\C-t" nil)

;; No accidental `evil-quit` call.
(define-key evil-window-map "q" nil)


;;;
;;; Custom general-use bindings (global-ish).
;;;

;; Comma as a Vim-style leader key.
;; Note: overriding the traditional binding of ",", which is
;;       "Repeat latest f, t, F or T in opposite direction".
;;
;; Motion state.
(define-prefix-command 'steve-comma-motion-map)
(define-key evil-motion-state-map "," 'steve-comma-motion-map)
(define-key steve-comma-motion-map "r" 'lusty-file-explorer)
(define-key steve-comma-motion-map "b" 'lusty-buffer-explorer)
;; (define-key steve-comma-motion-map "B" #'lusty-buffer-explorer)
(define-key steve-comma-motion-map "A" 'beginning-of-defun)
(define-key steve-comma-motion-map "p" 'fill-paragraph)
; STEVE rarely used vv
(define-key steve-comma-motion-map " " 'locate)

(define-key steve-comma-motion-map "x" ctl-x-map)
(define-key steve-comma-motion-map "xk" #'steve-kill-buffer)
;; (define-key steve-comma-motion-map "xo" #'ace-window)
;; (define-key steve-comma-motion-map "xo" #'vitreous)
(define-key steve-comma-motion-map "xO" #'vitreous-hydra/body)
(define-key steve-comma-motion-map "xg" #'magit-status)

(define-key steve-comma-motion-map "v" 'steve-vim-excursion)
(define-key steve-comma-motion-map "c" 'steve-comment-line-or-region)

(define-key steve-comma-motion-map "ea" #'steve-copy-register-unnamed-to-a)
(define-key steve-comma-motion-map "eb" #'steve-copy-register-unnamed-to-b)
(define-key steve-comma-motion-map "ec" #'steve-copy-register-unnamed-to-c)

(define-key steve-comma-motion-map "d." #'dumb-jump-go)
(define-key steve-comma-motion-map "d," #'dumb-jump-back)

(define-key steve-comma-motion-map "mj" #'bookmark-jump)
(define-key steve-comma-motion-map "ms" #'bookmark-set)

(define-key steve-comma-motion-map "h" #'steve-hydra-hideshow/body)

(defun steve-eval-region-and-close-visual-mode (beg end)
  (interactive "r")
  (eval-region beg end)
  (when (evil-visual-state-p)
    (setq deactivate-mark t)))


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


;; In normal mode I like ">" and "<" to operate immediately on the current
;; line, one key press, rather than wait for a motion. Delegate to the regular
;; operator for visual mode.
(require 'evil-types)  ;; For `(interactive "<v><vc>")`
(evil-define-operator steve-evil-shift-left (beg end &optional dummy-type count preserve-empty)
  :type line
  (interactive "<v><vc>")
  (steve-evil-shift-right beg end dummy-type (- (or count 1)) preserve-empty))
;;
(evil-define-operator steve-evil-shift-right (beg end &optional dummy-type count preserve-empty)
  :type line
  (interactive "<v><vc>")
  (if (and beg end)
      (evil-shift-right beg end count preserve-empty)
    (evil-shift-right-line count)))
;;
;; Make "=" behave in normal mode like it does in visual mode, acting
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

;; In Evil (and Vim), a `$` in visual mode will stretch the region to include
;; the trailing newline of the current line. But usually when I press `$` in
;; visual mode I only want the region to extend to the character preceding the
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
    (scroll-up count)
    (scroll-up count)
    (scroll-up count)))
;;
(evil-define-command steve-evil-scroll-line-up (count)
  "Scrolls the window 3 * COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (let ((scroll-preserve-screen-position nil))
    (scroll-down count)
    (scroll-down count)
    (scroll-down count)))
;;
(evil-define-command steve-evil-scroll-line-down-other (count)
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-other-window count)
  (scroll-other-window count)
  (scroll-other-window count))
;;
(evil-define-command steve-evil-scroll-line-up-other (count)
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-other-window-down count)
  (scroll-other-window-down count)
  (scroll-other-window-down count))

(evil-define-key*
  'motion 'global
  ;; (Overriding evil-scroll-line-up, evil-scroll-line-down)
  "\C-y" #'steve-evil-scroll-line-up
  "\C-e" #'steve-evil-scroll-line-down
  "\M-y" #'steve-evil-scroll-line-up-other
  "\M-e" #'steve-evil-scroll-line-down-other)

(evil-define-key*
  'normal 'global
  ;; Replaces `evil-change-whole-line', which is functionally the same as "cc".
  "S" #'save-buffer)

(evil-define-key*
  ;; STEVE instead of global should be the map for fundamental-mode (if one
  ;; existed).
  'insert 'global
  ;; Scroll-other in insert mode.
  "\M-y" #'steve-evil-scroll-line-up-other     ;; overrides `keyboard-quit`
  "\M-e" #'steve-evil-scroll-line-down-other)  ;; overrides `forward-sentence`

;;;
;;; Mode-specific bindings:
;;;

;;
;; Generally, C-SPC as a Vim-style leader key.

;; Help
(let ((temp-space-map (make-sparse-keymap)))
  (define-key temp-space-map "h" 'help-go-back)
  (define-key temp-space-map "l" 'help-go-forward)
  ;; Note: the *Help* buffer has position-specific bindings. E.g. activating a
  ;; link appears to be `help-follow`, but it's actually `push-button` in places
  ;; where it can actually do something.
  ;;
  ;; Note: A good alternative is `help-follow-symbol`, which works on more than
  ;; formal links.
  ;(define-key temp-space-map "." 'help-follow-symbol)
  (define-key temp-space-map "." 'push-button)
  (define-key temp-space-map "," 'help-go-back)
  (evil-define-key*
    '(motion normal) help-mode-map
    (kbd "C-SPC") temp-space-map))

;; Lusty Explorer
;; Have to do this in a hook because I wrote lusty-explorer.el in a weird way.
;; Note: only necessary because I've enabled evil in the minibuffer.
(add-hook 'lusty-setup-hook
          (lambda ()
            (evil-define-key*
              '(insert normal motion global) lusty-mode-map
              "\C-n" 'lusty-highlight-next
              "\C-p" 'lusty-highlight-previous
              "\C-s" 'lusty-highlight-next
              "\C-r" 'lusty-highlight-previous
              "\C-f" 'lusty-highlight-next-column
              "\C-b" 'lusty-highlight-previous-column)))

;; All programming modes
;; (Inherited by all/most other programming modes)
(let ((temp-space-map (make-sparse-keymap)))
  (define-key temp-space-map "," #'pop-tag-mark)
  (evil-define-key*
    '(motion normal) prog-mode-map
    (kbd "C-SPC") temp-space-map))

;; Emacs Lisp
(require 'ielm)  ;; so that ielm-map is defined
(dolist (elisp-related-map (list emacs-lisp-mode-map
                                 lisp-interaction-mode-map
                                 ielm-map))
  ;; All motion modes
  (let ((temp-space-map (make-sparse-keymap)))
    ;; (define-key temp-space-map "." 'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key temp-space-map "." #'elisp-def)
    (define-key temp-space-map "h" #'elisp-slime-nav-describe-elisp-thing-at-point)
    (define-key temp-space-map "e" 'eval-last-sexp)
    (define-key temp-space-map "k" 'eval-buffer)
    (define-key temp-space-map "K" 'eval-buffer)
    (define-key temp-space-map "i" #'steve-toggle-dp-on-sexp)
    (define-key temp-space-map "\C-i" #'steve-toggle-dp-on-sexp)
    (define-key temp-space-map "x" #'eval-defun)
    (define-key temp-space-map "M" #'macrostep-expand)
    (define-key temp-space-map "W" #'steve-hydra-elisp-refs/body)
    (define-key temp-space-map "?" #'steve-pp-eval-dwim)
    (evil-define-key*
      '(motion normal) elisp-related-map
      (kbd "C-SPC") temp-space-map))
  ;; Visual mode only
  (let ((temp-space-map (make-sparse-keymap)))
    (define-key temp-space-map "r" #'steve-eval-region-and-close-visual-mode)
    (evil-define-key*
      'visual elisp-related-map
      (kbd "C-SPC") temp-space-map)))

;; Grep
(evil-define-key
  '(motion normal) grep-mode-map
  ; (Instead of 'recompile.)
  "gg" 'evil-goto-first-line
  "q" 'kill-buffer-and-window
  "D" 'steve-remove-matching-lines)

;; Rust
(define-prefix-command 'steve-evil-rust-space-motion-map)
(define-key steve-evil-rust-space-motion-map "." 'racer-find-definition)
(define-key steve-evil-rust-space-motion-map "h" 'racer-describe)
(evil-define-key
  '(motion normal) rust-mode-map
  (kbd "C-SPC") steve-evil-rust-space-motion-map)


;;;
;;; Overrides of default states in some modes.
;;;

(evil-set-initial-state 'completion-list-mode 'motion)


;;;
;;; Misc
;;;

;; Treat "_" as a word character.
;(add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w")))
(modify-syntax-entry ?_ "w")


;; Vim-related utility functions.
;;

;; Note: could probably use `evil-edit` instead, but I'm used to this.
(defun e ()
  (interactive)
  (revert-buffer nil t))

