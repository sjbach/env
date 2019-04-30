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
(unless (file-exists-p "~/.emacs.d/evil")
  ;; `git clone https://github.com/emacs-evil/evil ~/.emacs.d/evil`
  (error "Clone `evil` to ~/.emacs.d/evil"))
;; From: https://github.com/emacs-evil/evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(unless (file-exists-p "~/.emacs.d/evil-collection")
  ;; `git clone https://github.com/emacs-evil/evil-collection.git ~/.emacs.d/evil-collection`
  (error "Clone `evil-collection` to ~/.emacs.d/evil-collection"))
;; Note: If this is placed after my custom bindings, they don't take effect.
;; From: https://github.com/emacs-evil/evil-collection
(add-to-list 'load-path "~/.emacs.d/evil-collection")
;; This evil-collection setting appears to pre-date `evil-want-minibuffer` in
;; the standard evil package, so I'm not clear whether or not it's redundant,
;; but I notice setting it makes the custom lusty-explorer bindings work
;; correctly (below).
(customize-set-variable 'evil-collection-setup-minibuffer t)
(require 'evil-collection)
;; Enable for all modes in the collection (until I decide that's a bad idea):
(evil-collection-init)


;;;
;;; Overrides of default evil bindings.
;;;

;; <SPACE> and <RET> don't need to be motion keys, as I never use them as such.
;; From: https://www.emacswiki.org/emacs/Evil
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; I don't use :ex mode (evil-ex).
(define-key evil-motion-state-map ":" 'execute-extended-command)
;; I don't use ';' for its traditional purpose ("Repeat latest f, t, F or T").
(define-key evil-motion-state-map ";" 'execute-extended-command)

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
(define-key steve-comma-motion-map "j" 'steve-juggle-previous-buffer)
(define-key steve-comma-motion-map "xo" 'other-window)
(define-key steve-comma-motion-map "x0" 'delete-window)
(define-key steve-comma-motion-map "x1" 'delete-other-windows)
(define-key steve-comma-motion-map "x2" 'split-window-vertically)
(define-key steve-comma-motion-map "x3" 'split-window-horizontally)
(define-key steve-comma-motion-map "xk" 'kill-this-buffer)
; STEVE rarely used vv
(define-key steve-comma-motion-map "v" 'steve-vim-excursion)
(define-key steve-comma-motion-map "A" 'beginning-of-defun)
(define-key steve-comma-motion-map "p" 'fill-paragraph)
(define-key steve-comma-motion-map "h" 'ff-find-other-file)
(define-key steve-comma-motion-map " " 'locate)
;;
;; Visual state.
(define-prefix-command 'steve-comma-visual-map)
(define-key evil-visual-state-map "," 'steve-comma-visual-map)
(define-key steve-comma-visual-map "c" 'comment-dwim)

(evil-define-key
  'motion 'global
  ;; I don't use TAB for its traditional purpose.
  (kbd "TAB") 'steve-juggle-previous-buffer

  ;; Close buffer (instead of scroll down).
  "\C-d" 'steve-close-buffer-and-window-unless-last
  ;; I like C-y and C-e to scroll faster.
  "\C-y" (lambda ()
           (interactive)
           (evil-scroll-line-up 3))
  "\C-e" (lambda ()
           (interactive)
           (evil-scroll-line-down 3)))


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
    (evil-define-key
      '(motion normal) elisp-related-map
      " " temp-space-map))
  (let ((temp-space-map (make-sparse-keymap)))
    (define-key temp-space-map "r" 'eval-region)
    (evil-define-key
      'visual elisp-related-map
      " " temp-space-map)))

;; Grep
(evil-define-key
  '(motion normal) grep-mode-map
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

;;;
;;; Overrides of default states in some modes.
;;;

;(setq evil-emacs-state-modes
;      (remq 'completion-list-mode evil-emacs-state-modes))
(evil-set-initial-state 'completion-list-mode 'motion)


;;;
;;; Vim-related utility functions.
;;;

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

