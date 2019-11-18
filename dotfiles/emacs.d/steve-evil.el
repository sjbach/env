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

;;;
;;; Overrides of default evil bindings.
;;;

;; Evil mappings should already be loaded now; don't want to use
;; with-eval-after-load and confuse the control flow.
(cl-assert (featurep 'evil-maps))

;; <SPACE> and <RET> don't need to be motion keys, as I never use them as such.
;; From: https://www.emacswiki.org/emacs/Evil
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; Switch the mappings for "'" and "`"; evil-goto-mark is better in almost
;; every case, so best that it's more accessible
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; I don't use :ex mode (evil-ex).
(define-key evil-motion-state-map ":" 'execute-extended-command)
;; Shift-meta-: is awkward.
(define-key evil-motion-state-map "\M-;" #'eval-expression)
;; I don't use ';' for its traditional purpose ("Repeat latest f, t, F or T").
; (define-key evil-motion-state-map ";" 'execute-extended-command)

(evil-define-key
  'normal 'global
  ;; I see these bindings as risky. In usual Emacs usage, quitting is quite
  ;; rare. I never want it to happen willy-nilly. So unset them.
  "ZZ" nil
  "ZQ" nil
  ;; I use different bindings for tag stuff.
  "\C-t" nil)


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
;(define-key steve-comma-motion-map "j" 'steve-juggle-previous-buffer)

(define-key steve-comma-motion-map "x" ctl-x-map)
(define-key steve-comma-motion-map "xk" #'steve-kill-buffer)

(define-key steve-comma-motion-map "v" 'steve-vim-excursion)
(define-key steve-comma-motion-map "c" 'steve-comment-line-or-region)

(define-key steve-comma-motion-map "ea" #'steve-copy-register-unnamed-to-a)
(define-key steve-comma-motion-map "eb" #'steve-copy-register-unnamed-to-b)
(define-key steve-comma-motion-map "ec" #'steve-copy-register-unnamed-to-c)
;
;(define-key steve-comma-motion-map "o\C-e"
;  (lambda () (interactive) (scroll-other-window 3)))
;(define-key steve-comma-motion-map "o\C-y"
;  (lambda () (interactive) (scroll-other-window-down 3)))
;(define-key steve-comma-motion-map "of" #'scroll-other-window)
;(define-key steve-comma-motion-map "ob" #'scroll-other-window-down)
; STEVE rarely used vv
(define-key steve-comma-motion-map "A" 'beginning-of-defun)
(define-key steve-comma-motion-map "p" 'fill-paragraph)
(define-key steve-comma-motion-map "h" 'ff-find-other-file)
(define-key steve-comma-motion-map " " 'locate)

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
;; TODO: should I be using `evil-define-key` for this instead?
(define-key evil-normal-state-map "<" #'steve-evil-shift-left)
(define-key evil-normal-state-map ">" #'steve-evil-shift-right)


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

(evil-define-key
  'motion 'global
  ;; I don't use TAB for its traditional purpose.
  (kbd "TAB") 'steve-juggle-previous-buffer
  ;; Close buffer (instead of scroll down).
  "\C-d" 'steve-close-buffer-and-window-unless-last
  ;; (Instead of evil-scroll-line-up, evil-scroll-line-up.)
  "\C-y" 'steve-evil-scroll-line-up
  "\C-e" 'steve-evil-scroll-line-down)

(evil-define-key
  ;; STEVE instead of global should be the map for fundamental-mode (if one
  ;; existed).
  'insert 'global
  ;; Mapping C-d in insert mode to close buffer feels risky to me, but I want
  ;; it in at least one place, so special case it.
  "\C-d" #'(lambda ()
             (interactive)
             (if (string-equal (buffer-name) steve--temp-paste-buf-name)
                 (steve-close-buffer-and-window-unless-last)
               ;; Default binding (though I never actually use this).
               ; STEVE vv this path causes an error
               (evil-shift-left-line 1))))

;;;
;;; Mode-specific bindings:
;;;

;;
;; Generally, SPC as a Vim-style leader key.

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
  (evil-define-key
    '(motion normal) help-mode-map
    (kbd "TAB") 'steve-juggle-previous-buffer
    ;(kbd "C-h") 'help-go-back
    ;(kbd "C-l") 'help-go-forward
    " " temp-space-map))

;; Lusty Explorer
;; Have to do this in a hook because I wrote lusty-explorer.el in a weird way.
;; Note: only necessary because I've enabled evil in the minibuffer.
(add-hook 'lusty-setup-hook
          (lambda ()
            (evil-define-key
              ;'(insert normal) lusty-mode-map
              '(insert normal motion global) lusty-mode-map
              "\C-n" 'lusty-highlight-next
              "\C-p" 'lusty-highlight-previous
              "\C-s" 'lusty-highlight-next
              "\C-r" 'lusty-highlight-previous
              "\C-f" 'lusty-highlight-next-column
              "\C-b" 'lusty-highlight-previous-column)))

;; Emacs Lisp
(dolist (elisp-related-map (list emacs-lisp-mode-map
                                 lisp-interaction-mode-map))
  (let ((temp-space-map (make-sparse-keymap)))
    ;; Note: I think the glue for this is provided by the `elisp-slime-nav`
    ;; package.
    ;(define-key temp-space-map "." 'xref-find-definitions)
    ;(define-key temp-space-map "," 'xref-pop-marker-stack)
    (define-key temp-space-map "." 'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key temp-space-map "," 'pop-tag-mark)
    (define-key temp-space-map "d" 'elisp-slime-nav-describe-elisp-thing-at-point)
    (define-key temp-space-map "e" 'eval-last-sexp)
    (define-key temp-space-map "k" 'eval-buffer)
    (define-key temp-space-map "K" 'eval-buffer)
    (define-key temp-space-map "i" #'steve-toggle-dp-on-sexp)
    (define-key temp-space-map "\C-i" #'steve-toggle-dp-on-sexp)
    (define-key temp-space-map "x" #'eval-defun)
    (evil-define-key
      '(motion normal) elisp-related-map
      " " temp-space-map))
  (let ((temp-space-map (make-sparse-keymap)))
    (define-key temp-space-map "r" #'steve-eval-region-and-close-visual-mode)
    (evil-define-key
      'visual elisp-related-map
      " " temp-space-map)))

;; Grep
(evil-define-key
  '(motion normal) grep-mode-map
  ; (Instead of 'compilation-next-error.)
  (kbd "TAB") 'steve-juggle-previous-buffer
  ; (Instead of 'recompile.)
  "gg" 'evil-goto-first-line
  "q" 'steve-close-buffer-and-window-unless-last
  "D" 'steve-remove-matching-lines)

;; Rust
(define-prefix-command 'steve-evil-rust-space-motion-map)
(define-key steve-evil-rust-space-motion-map "." 'racer-find-definition)
(define-key steve-evil-rust-space-motion-map "," 'pop-tag-mark)
(define-key steve-evil-rust-space-motion-map "d" 'racer-describe)
(define-key steve-evil-rust-space-motion-map "h" 'racer-describe)
(evil-define-key
  '(motion normal) rust-mode-map
  " " steve-evil-rust-space-motion-map)
(evil-define-key
  'insert rust-mode-map
  (kbd "TAB") 'company-indent-or-complete-common
  "\C-n" 'company-select-next
  "\C-p" 'company-select-previous
  (kbd "<right>") 'company-complete)

;; Rust/cargo compilation
(evil-define-key
  '(motion normal) cargo-process-mode-map
  ;; (Rather than next compilation error.)
  (kbd "TAB") 'steve-juggle-previous-buffer
  )

;;;
;;; Overrides of default states in some modes.
;;;

;(setq evil-emacs-state-modes
;      (remq 'completion-list-mode evil-emacs-state-modes))
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

;; Note: could probably use `evil-write` instead, but I'm used to this.
(defun w (&optional args)
  (interactive "p")
  (save-buffer args))

;(defun q (&optional args)
;  (interactive "P")
;  (save-buffers-kill-emacs args))
;
;(defun wq (&optional args)
;  (interactive "P")
;  (save-buffers-kill-emacs args))


; Notes:
; - "If you want the underscore to be recognised as word character, you can modify its entry in the syntax-table:
;  (modify-syntax-entry ?_ "w")
;  This gives the underscore the word syntax-class. You can use a mode-hook to modify the syntax-table in all buffers of some mode, e.g.:
;  (add-hook 'c-mode-common-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;  This gives the underscore the word syntax-class in all C-like buffers."
; - evil-vars.el -- config some behavior of evil
; - C-z to switch back-and-forth to Emacs mode
; - Binding shift, e.g. C-S-l, doesn't work in the terminal.

; Preconditions:
; - git clone https://github.com/emacs-evil/evil ~/.emacs.d/evil
; - git clone https://github.com/emacs-evil/evil-collection.git ~/.emacs.d/evil-collection

; References:
; - https://github.com/noctuid/evil-guide

