;;; -*- lexical-binding: t; -*-
;;
;; Key translation, key maps, key definitions
;;
;; Note: other files also contain key maps and key definitions.
;;

(require 'hydra)
(require 'hercules)

;;;
;;; Custom input sequences
;;;

;; Terminal Emacs natively recognizes many (all?) of the extended input
;; sequences generated by xterm with `modifyOtherKeys` and `formatOtherKeys`,
;; but those two xterm settings in their safest enabled condition purposely
;; skip a bunch of useful chords. There is reasoning behind this. I still want
;; some of those chords, though. xterm and iTerm2 allow custom input sequences,
;; so I configure those terminals to emit the ugly sequences below and
;; reinterpret them here to their true keys.
;;
;; Someday there will be sanity in terminal input; until then, the below
;; suffices.

(let ((input-map (if (boundp 'input-decode-map)
                     input-decode-map
                   function-key-map)))
  ;; Reminder: these definitions are pointless without a corresponding setup in
  ;; your terminal emulator.
  (define-key input-map "\e[STEVE-C-SPC" (kbd "C-SPC"))
  (define-key input-map "\e[STEVE-C-S-SPC" (kbd "C-S-SPC"))
  (define-key input-map "\e[STEVE-S-RET" [S-return])
  (define-key input-map "\e[STEVE-C-RET" [C-return])
  (define-key input-map "\e[STEVE-C-S-RET" [C-S-return])
  (define-key input-map "\e[STEVE-S-DEL" (kbd "S-DEL"))
  (define-key input-map "\e[STEVE-C-DEL" (kbd "C-DEL"))
  (define-key input-map "\e[STEVE-C-S-DEL" (kbd "C-S-DEL"))
  (define-key input-map "\e[STEVE-C-;" (kbd "C-;"))
  (define-key input-map "\e[STEVE-C-/" (kbd "C-/"))
  (define-key input-map "\e[STEVE-C-'" (kbd "C-'"))
  (define-key input-map "\e[STEVE-C-," (kbd "C-,"))
  (define-key input-map "\e[STEVE-C-." (kbd "C-."))
  (define-key input-map "\e[STEVE-C--" (kbd "C--"))
  (define-key input-map "\e[STEVE-C-S-'" (kbd "C-S-'"))
  (define-key input-map "\e[STEVE-C-S-t" (kbd "C-S-t"))
  (define-key input-map "\e[STEVE-C-S-s" (kbd "C-S-s"))
  (define-key input-map "\e[STEVE-C-S-o" (kbd "C-S-o"))
  ;; STEVE Not yet set in iTerm2 b/c it is a macOS binding.
  (define-key input-map "\e[STEVE-C-TAB" (kbd "C-TAB"))
  ;; Note: C-- --> `negative-argument`
  )


;;;
;;; Custom context-free global bindings
;;;

;; No good reason for quitting Emacs to be a convenient key sequence.
(define-key ctl-x-map (kbd "C-c") nil)
(define-key ctl-x-map (kbd "C-c C-x C-c C-x C-c") #'save-buffers-kill-terminal)

;; Remap some function keys
;;
;; Instead of `kmacro-start-macro-or-insert-counter'.
(global-set-key [f3] #'pluck-other-buffer)
;; Instead of `kmacro-end-or-call-macro'.
(global-set-key [f4] #'other-window)
(global-set-key [S-f4] (kbd "C-u - <f4>"))
(global-set-key [f5] #'other-frame)
(global-set-key [S-f5] (kbd "C-u - <f5>"))

;; Buffer management
;;
;; Rather than `flyspell-auto-correct-previous-word'.
(global-set-key (kbd "C-;") #'pluck-other-buffer)
;;
;; Aside: `DEL` refers to the backspace key; The Delete key is
;; `delete`/`<deletechar>`.
(global-set-key (kbd "C-DEL") #'steve-kill-buffer)
(global-set-key (kbd "C-S-DEL") #'kill-buffer-and-window)
;;
(define-key help-map "H" #'steve-show-help-buffer)

;; Window management
;;
;; Change the active window with Shift-<arrow key>.
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around nil)
;;
;; (global-set-key (kbd "C-'") #'vitreous-other-window)
(global-set-key (kbd "C-'") #'vitreous-dwim)
;; (global-set-key (kbd "C-S-'") (kbd "C-u - C-'"))
(global-set-key (kbd "C-S-'") #'vitreous-hydra/body)

;; Frame management
;; (global-set-key (kbd "C-\\") #'ef-other-frame)  ;; overrides toggle-input-method
;; (global-set-key [C-return] #'ef-other-frame)
(global-set-key [C-return] #'pluck-other-errand-frame)
;;
;; I use frames as pseudo-workspaces, I don't want to delete them all
;; accidentally because of a mistype.
(define-key ctl-x-5-map "1"
  (lambda (&optional dummy)
    (interactive)
    (message "Run `M-x delete-other-frames'")))

(global-set-key (kbd "C-S-s") #'steve-jump-to-scratch)

;; Magit defines `magit-status' on "C-x g" explicitly rather than on
;; `ctl-x-map' preventing it from being accessed through other methods of
;; reaching `ctl-x-map'.
(define-key ctl-x-map "g" #'magit-status)


;; A global prefix key: C-SPC.
;;
;; Established for context-free commands having the spirit of commanding Emacs
;; to do something that doesn't relate to the content of the current buffer,
;; e.g. window management, buffer switching. Always defined/available.
;;
(define-prefix-command 'steve-C-SPC-map)
;; Note: overrides `C-@' -> `set-mark-command'.
(global-set-key (kbd "C-SPC") steve-C-SPC-map)

(define-key steve-C-SPC-map "x" ctl-x-map)
(define-key steve-C-SPC-map (kbd "C-f") 'lusty-file-explorer)
(define-key steve-C-SPC-map (kbd "C-b") 'lusty-buffer-explorer)
(define-key steve-C-SPC-map "b" 'lusty-buffer-explorer)
(define-key steve-C-SPC-map "d" #'toggle-debug-on-error)
;; (define-key steve-C-SPC-map "v" 'steve-vim-excursion)

;; Filename search:
(define-prefix-command 'steve-filename-search-prefix-map)
(define-key steve-C-SPC-map "f" steve-filename-search-prefix-map)
;; (Filled in elsewhere.)

;; File grep:
(define-prefix-command 'steve-grep-prefix-map)
(define-key steve-C-SPC-map "g" steve-grep-prefix-map)
;; (Filled in elsewhere.)

;; Helpful:
(let ((prefix-map (make-sparse-keymap)))
  (define-key prefix-map (kbd "C-c") #'helpful-callable)
  (define-key prefix-map (kbd "C-f") #'helpful-function)
  (define-key prefix-map (kbd "C-m") #'helpful-macro)
  (define-key prefix-map (kbd "C-c") #'helpful-command)
  (define-key prefix-map (kbd "C-k") #'helpful-key)
  (define-key prefix-map (kbd "C-v") #'helpful-variable)
  (define-key prefix-map (kbd "C-.") #'helpful-at-point)
  (define-key steve-C-SPC-map (kbd "C-h") prefix-map))


;;;
;;; Custom mode-specific bindings
;;;

;; A global prefix key: "C-,".
;;
;; Established for commands that relate in some way to the
;; code/content/properties of the current buffer. Perhaps mode-related, perhaps
;; buffer-local. Available in all Evil modes (normal, insert, Emacs). Often
;; relating to the cursor position or active region.
;;
(defvar steve-mode-specific-prefix-key (kbd "C-,"))
;;
;; Posterity:
;; - Many "C-," mappings are not placed on this map but rather on encapsulating
;;   maps.
;; - Also accessible as "," in Evil normal mode. (But without the "C-," bindings
;;   set outside this map.)
(define-prefix-command 'steve-C-comma-map)
(global-set-key steve-mode-specific-prefix-key steve-C-comma-map)

(define-key steve-C-comma-map "x" ctl-x-map)
(define-key steve-C-comma-map "c" 'steve-comment-line-or-region)
(define-key steve-C-comma-map "A" 'beginning-of-defun)
(define-key steve-C-comma-map "p" 'fill-paragraph)

(define-key steve-C-comma-map "ea" 'steve-copy-register-unnamed-to-a)
(define-key steve-C-comma-map "eb" 'steve-copy-register-unnamed-to-b)
(define-key steve-C-comma-map "ec" 'steve-copy-register-unnamed-to-c)

(define-key steve-C-comma-map "d." #'dumb-jump-go)
(define-key steve-C-comma-map "d," #'dumb-jump-back)

(define-key steve-C-comma-map "mj" #'bookmark-jump)
(define-key steve-C-comma-map "ms" #'bookmark-set)

(define-key steve-C-comma-map "h" #'steve-hydra-hideshow/body)


;; Help:
(let ((prefix-map (make-sparse-keymap)))
  (define-key prefix-map "h" 'help-go-back)
  (define-key prefix-map "l" 'help-go-forward)
  ;; Note: the *Help* buffer has position-specific bindings. E.g. activating a
  ;; link appears to be `help-follow`, but it's actually `push-button` in places
  ;; within the buffer where it can actually do something.
  ;;
  ;; Note: A good alternative is `help-follow-symbol`, which works on more than
  ;; formal links.
  (define-key prefix-map "." 'push-button)   ;; Duplicate: ",."
  (define-key prefix-map "," 'help-go-back)  ;; Duplicate: ",,"
  (define-key help-mode-map steve-mode-specific-prefix-key prefix-map))
;;
;; Clear out some bindings from `help-map' to make its `which-key' window
;; a little easier to scan.
(define-key help-map "\C-a" nil)
(define-key help-map "\C-c" nil)
(define-key help-map "\C-e" nil)
(define-key help-map "\C-m" nil)
(define-key help-map "\C-o" nil)
(define-key help-map "\C-s" nil)
(define-key help-map "\C-w" nil)
;;
(define-key help-map "g" nil)
(define-key help-map "4i" nil)
(define-key help-map "n" nil)
(define-key help-map "t" nil)

;; Programming modes:
(let ((prefix-map (make-sparse-keymap)))
  (define-key prefix-map "," 'pop-tag-mark)
  ;; (A keymap inhereited by most programming language modes.)
  (define-key prog-mode-map steve-mode-specific-prefix-key prefix-map))

;; Emacs Lisp:
(require 'ielm)  ;; so that ielm-map is defined
(dolist (elisp-related-map (list emacs-lisp-mode-map
                                 lisp-interaction-mode-map
                                 ielm-map))
  (let ((prefix-map (make-sparse-keymap)))
    ;; (define-key prefix-map "." 'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key prefix-map "." #'elisp-def)
    (define-key prefix-map "h" #'elisp-slime-nav-describe-elisp-thing-at-point)
    (define-key prefix-map "e" 'eval-last-sexp)
    (define-key prefix-map "k" 'eval-buffer)  ;; STEVE more sensible binding
    (define-key prefix-map "K" 'eval-buffer)  ;; STEVE more sensible binding
    (define-key prefix-map "i" #'steve-toggle-dp-on-sexp)
    (define-key prefix-map "\C-i" #'steve-toggle-dp-on-sexp)
    ;; STEVE fix: overrides ctl-x-map
    (define-key prefix-map "x" #'eval-defun)
    (define-key prefix-map "M" #'macrostep-expand)
    (define-key prefix-map "W" #'steve-hydra-elisp-refs/body)
    (define-key prefix-map "?" #'steve-pp-eval-dwim)
    ;; (Actually visual-mode only)
    (define-key prefix-map "r" #'steve-eval-region-and-close-visual-mode)
    (define-key elisp-related-map steve-mode-specific-prefix-key prefix-map)))

;; Grep
(require 'grep)  ;; so that `grep-mode-map' is defined
(let ((prefix-map (make-sparse-keymap)))
  (define-key prefix-map "D" 'steve-remove-matching-lines)
  (define-key grep-mode-map steve-mode-specific-prefix-key prefix-map))

;; Rust
(require 'rust-mode)
(let ((prefix-map (make-sparse-keymap)))
  (define-key prefix-map "." 'racer-find-definition)
  (define-key prefix-map "h" 'racer-describe)
  (define-key rust-mode-map steve-mode-specific-prefix-key prefix-map))

;; Magit
(let ((prefix-map (make-sparse-keymap)))
  (define-key prefix-map "1" #'magit-section-show-level-1-all)
  (define-key prefix-map "2" #'magit-section-show-level-2-all)
  (define-key prefix-map "3" #'magit-section-show-level-3-all)
  (define-key prefix-map "4" #'magit-section-show-level-4-all)
  (define-key magit-mode-map steve-mode-specific-prefix-key prefix-map))


;;;
;;; Overrides
;;;

;; Instead of `undo' or `undo-tree-undo'. Set on `global-map' in addition to
;; `evil-normal-state-map' because at times when Evil is unavailable I don't
;; want to be surprised by the fallback keybinding.
(global-set-key (kbd "C-/") 'steve-remove-evil-search-highlight)

;; Instead of `kill-buffer'.
(define-key ctl-x-map "k" #'steve-kill-buffer)
;; Instead of `other-window'.
;; (define-key ctl-x-map "o" #'ace-window)
(define-key ctl-x-map "o" #'vitreous-other-window)
;; Instead of translating to `other-window'.
(define-key ctl-x-map "O" #'vitreous-hydra/body)

(require 'flyspell)
;; I use flyspell in buffers that use these keys for other things.
;; Instead of `flyspell-auto-correct-previous-word'.
(define-key flyspell-mode-map [(control ?\;)] nil)
;; Instead of `flyspell-goto-next-error'.
(define-key flyspell-mode-map [(control ?\,)] nil)
;; Instead of `flyspell-auto-correct-word'.
(define-key flyspell-mode-map [(control ?\.)] nil)

;; Because it confuses me when I accidentally press M-v as terminal paste.
(global-unset-key (kbd "M-v"))  ; default: `scroll-down-command'

;;;
;;; Hydras
;;;

(setq hydra-look-for-remap t)

;; Hideshow
(defhydra steve-hydra-hideshow (:pre (hs-minor-mode 1)
                                :foreign-keys run)
   ("h" hs-hide-all "hide all" :column "Hide")
   ;; ("d" hs-hide-block "hide block")
   ;; ("l" hs-hide-level "hide level")
   ("s" hs-show-all "show all" :column "Show")
   ;; ("a" hs-show-block "show block")
   ("t" hs-toggle-hiding "toggle" :column "Toggle")
   ("q" nil))

;; Elisp-ref
(defhydra steve-hydra-elisp-refs ()
  "\nelisp-refs: <%s(symbol-at-point)>\n"
   ("a" (elisp-refs-symbol (symbol-at-point)) "all syms")
   ("f" (elisp-refs-function (symbol-at-point)) "funcs")
   ("m" (elisp-refs-macro (symbol-at-point)) "macros")
   ("v" (elisp-refs-variable (symbol-at-point)) "vars")
   ("s" (elisp-refs-special (symbol-at-point)) "specials")
   ("q" nil))

;; Macrostep
(hercules-def
 :toggle-funs #'macrostep-mode
 :keymap 'macrostep-keymap)

