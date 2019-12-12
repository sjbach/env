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
;; but in their safest enabled condition those two xterm settings purposely
;; skip a bunch of useful chords. There is reasoning behind this. I still want
;; some of those chords. xterm and iTerm2 allow custom input sequences, so I
;; configure those terminals to emit the ugly sequences below and reinterpret
;; them here to their true keys.
;;
;; Someday there will be sanity in terminal input; until then, the below
;; suffices.

(let ((input-map (if (boundp 'input-decode-map)
                     input-decode-map
                   function-key-map)))
  ;; Reminder: these are pointless without a corresponding setup in your
  ;; terminal emulator.
  (define-key input-map "\e[STEVE-C-SPC" (kbd "C-SPC"))
  (define-key input-map "\e[STEVE-C-S-SPC" (kbd "C-S-SPC"))
  (define-key input-map "\e[STEVE-S-RET" [S-return])
  (define-key input-map "\e[STEVE-C-RET" [C-return])
  (define-key input-map "\e[STEVE-C-S-RET" [C-S-return])
  (define-key input-map "\e[STEVE-S-DEL" (kbd "S-DEL"))
  (define-key input-map "\e[STEVE-C-DEL" (kbd "C-DEL"))
  (define-key input-map "\e[STEVE-C-S-DEL" (kbd "C-S-DEL"))
  (define-key input-map "\e[STEVE-C-/" (kbd "C-/"))
  (define-key input-map "\e[STEVE-C-'" (kbd "C-'"))
  (define-key input-map "\e[STEVE-C-," (kbd "C-,"))
  (define-key input-map "\e[STEVE-C-." (kbd "C-."))
  (define-key input-map "\e[STEVE-C--" (kbd "C--"))
  (define-key input-map "\e[STEVE-C-S-'" (kbd "C-S-'"))
  ;; STEVE Not yet set in iTerm2 b/c it is a macOS binding.
  (define-key input-map "\e[STEVE-C-TAB" (kbd "C-TAB"))
  ;; Note: C-- --> `negative-argument`
  )


;;;
;;; Context-free global bindings
;;;

;; No good reason for quitting Emacs to be a convenient key sequence.
(define-key ctl-x-map (kbd "C-c") nil)
(define-key ctl-x-map (kbd "C-c C-x C-c C-x C-c") #'save-buffers-kill-terminal)

;; Remap some function keys
;;
;; Instead of kmacro-start-macro-or-insert-counter
(global-set-key [f3] 'steve-juggle-previous-buffer)
;; Instead of kmacro-end-or-call-macro
(global-set-key [f4] #'other-window)
(global-set-key [S-f4] (kbd "C-u - <f4>"))
(global-set-key [f5] #'other-frame)
(global-set-key [S-f5] (kbd "C-u - <f5>"))

;; Buffer management
;;
;; (global-set-key [C-return] 'steve-juggle-previous-buffer)
(global-set-key [C-return] #'rummage-other-buffer)
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
;; (setq windmove-wrap-around t)
(setq windmove-wrap-around nil)
;;
(global-set-key (kbd "C-'") #'vitreous-other-window)
(global-set-key (kbd "C-S-'") (kbd "C-u - C-'"))

;; Frame management
(global-set-key (kbd "C-\\") #'cf-other-frame)  ;; overrides toggle-input-method
;;
;; I use frames as pseudo-workspaces, I don't want to delete them all
;; accidentally because of a mistype.
(define-key ctl-x-5-map "1"
  (lambda (&optional dummy)
    (interactive)
    (message "Run `M-x delete-other-frames'")))


;;;
;;; Hydras
;;;

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

