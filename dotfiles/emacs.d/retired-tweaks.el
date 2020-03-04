;; This file isn't loaded, it's just for posterity / searchability.
(cl-assert false)


;; Tab characters displayed as 2 columns instead of 8.
(setq-default tab-width 2)
;; Disabled because it looks bad in any code with tabs that expects a tab is 8
;; spaces.

;; Ace window
(require 'ace-window)
;; In terminal Emacs you only view one frame at a time, so a global scope is
;; not usually what you want.
(setq aw-scope 'frame)  ;; vs global (all frames)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(setq which-key-max-description-length 70)  ;; default: 27
(setq which-key-show-early-on-C-h t)  ;; default: nil

(evil-define-key*
  'insert 'global
  ;; Mapping C-d in insert mode to close buffer feels risky to me, but I want
  ;; it in at least one place, so special case it.
  "\C-d" #'(lambda ()
             (interactive)
             (if (string-equal (buffer-name) steve--temp-paste-buf-name)
                 (kill-buffer-and-window)
               ;; Default binding (though I never actually use this).
               ; STEVE vv this path causes an error
               (evil-shift-left-line 1))))

;; Rust/cargo/company stuff
;;
;; (redundant with default bindings)
(evil-define-key*
  'insert rust-mode-map
  (kbd "TAB") 'company-indent-or-complete-common
  "\C-n" 'company-select-next
  "\C-p" 'company-select-previous
  (kbd "<right>") 'company-complete)
;;
(evil-define-key
  '(motion normal) cargo-process-mode-map
  ;; (Rather than next compilation-next-error in compilation-mode-map.)
  ;(kbd "TAB") 'steve-juggle-previous-buffer
  ;(kbd "TAB") 'cargo-next
  "\t" 'compilation-next-error)

;; See `macrostep-expand` instead, or just `pp-macroexpand-last-sexp`.
(defun steve-show-macroexpansion-for-region (beg end)
  (interactive "r")
  (unless (and beg end)
    (error "No region given"))
  (let* ((s (buffer-substring-no-properties beg end))
         (sexp (read s))
         (macroexpanded (macroexpand-1 sexp))
         (buf-name "*Steve-Macroexpanded*")
         (temp-buffer-setup-hook '(emacs-lisp-mode)))
    (with-output-to-temp-buffer buf-name
      (pp macroexpanded))))

(global-set-key (kbd "C-S-L") 'latex-preview-pane-mode)

(setq evil-emacs-state-modes
      (remq 'completion-list-mode evil-emacs-state-modes))

;; Mostly redundant with the native kill-buffer-and-window
(defun steve-close-buffer-and-window-unless-last ()
  (interactive)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (next (next-window window)))
    (kill-buffer buffer)
    (when (and window
               (not (eq window next)))
      (delete-window window))))

;; Different color for parens
(require 'paren-face)
(global-paren-face-mode 1)

;; *scratch* is preserved in a different way (below).
(setq desktop-files-not-to-save
      (rx (or (regexp desktop-files-not-to-save)
              (seq string-start
                   (literal (buffer-file-name (remember-notes)))
                   string-end)
              (seq string-start
                   "*scratch*"
                   string-end))))

;; <SPACE> and <RET> don't need to be motion keys, as I never use them as such.
;; From: https://www.emacswiki.org/emacs/Evil
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")


;; Save/restore *scratch* across sessions.
;;
;; The documentation for remember-notes says: "Set ‘initial-buffer-choice’ to
;; ‘remember-notes’ to visit your notes buffer when Emacs starts.  Set
;; ‘remember-notes-buffer-name’ to "*scratch*" to turn the *scratch* buffer
;; into your notes buffer."
;;
;; This appears to be incorrect - if *scratch* exists when remember-notes is
;; called (and it always will exist), then the notes buffer won't be renamed to
;; *scratch*. To work around this, we have to kill the *scratch* buffer before
;; remember-notes is called. Emacs automatically recreates the *scratch* buffer
;; with the correct modes, so not a big deal, but hacky.
(setq remember-notes-buffer-name "*scratch*")
(setq initial-buffer-choice #'remember-notes)
(setq initial-buffer-choice
      (lambda ()
        (kill-buffer remember-notes-buffer-name)  ; *scratch*
        (prog1
            (remember-notes)
          ;; Don't let *scratch* be closed accidentally.
          (with-current-buffer remember-notes-buffer-name
            (emacs-lock-mode 'kill)))))

;; Defaults for these Racer variables are fine.
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/rust/src")

;; There are better options than icomplete these days.
(require 'icomplete)
(icomplete-mode 1)
(setq icomplete-compute-delay 0.1)

;; "In some circumstances, you may want to load packages explicitly in your
;; init file (usually because some other code in your init file depends on a
;; package). In that case, your init file should call the function
;; package-initialize."
(package-initialize)
(setq package-enable-at-startup nil) ; (because we just now did it.)

;; Make just-in-time font-lock styling be more aggressive. For me this results
;; in fairly constant 5-10% average CPU load when styling a very large
;; buffer. That's fine.
(setq jit-lock-stealth-time 0.5)
(setq jit-lock-stealth-nice 0.1)

(when (file-exists-p "~/clojure")
  ;; TODO: cleanup
  ;(add-to-list 'load-path "~/clojure/swank-clojure")
  ;(setq swank-clojure-jar-path "~/clojure/clojure-git/clojure.jar")
  ;(require 'swank-clojure-autoload)

;  (eval-after-load "slime"
;    '(add-to-list 'slime-lisp-implementations '(sbcl ("sbcl"))))

  (add-to-list 'load-path "~/clojure/clojure-mode")
  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode)))

;; force horizontal splits - stolen from stackoverflow somewhere
(setq split-height-threshold
      (if (>= emacs-major-version 23)
        nil
        999))
(setq split-width-threshold 0)

;; Try to prevent lots of window splitting.  Stolen with modification from here
;; http://stackoverflow.com/questions/1381794/too-many-split-screens-opening-in
;;
(defun steve--display-buffer-fn (buffer current-window-unacceptable-p)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or current-window-unacceptable-p
               (not (eq (window-buffer (selected-window))
                        buffer)))
           ;; (> (frame-width) 162)
           )
      (split-window-horizontally))
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer'
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
    ;; (display-buffer buffer current-window-unacceptable-p)))
    (display-buffer buffer nil)))
(setq display-buffer-function #'steve--display-buffer-fn)

;; Unexpected new windows are annoying.
(setq pop-up-windows nil)

;; Slime autodoc used to expand the minibuffer height, which was annoying.
(setq resize-mini-windows nil)

;; Turn on font-lock mode for syntax highlighting
;; (These are now defaults)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Transient mark mode is now a default (and I think this was accidentally
;; disabling it, not enabling it).
(transient-mark-mode t)

;; I mostly use Emacs in a terminal these days and with my color choices the
;; default theme is good enough.
;;
(require 'color-theme)
;(color-theme-xemacs)
(color-theme-initialize)
(let ((display (getenv "DISPLAY")))
  (if (and display (> (length display) 0))
      ;; X
      (color-theme-robin-hood)
    ;; Terminal
    ;(color-theme-dark-laptop)))
    ;(color-theme-comidia)))
    ;(progn
    ;  (load-theme 'ample t t)
    ;  (enable-theme 'ample))))
;    (color-theme-comidia)))
    (color-theme-dark-laptop)))
;(color-theme-sitaramv-solaris)

;; (Font chosen through terminal instead.)
(when (and (>= emacs-major-version 23)
           (x-display-list))
  (let ((font
          (cond ((x-list-fonts "Bitstream Vera Sans Mono-9")
                  "Bitstream Vera Sans Mono-9")
                ((string-equal system-type "darwin")
                 "Dejavu Sans Mono-12")
                (t
                 "Dejavu Sans Mono-9"))))
    (if (or (> emacs-major-version 23)
            (and (eq emacs-major-version 23)
                 (>= emacs-minor-version 1)))
      (set-frame-font font)
      (set-default-font font))))

;; Shorten paths in grep-mode
;; Disabled now that I'm not working in java as much
(require 'scf-mode)
(add-hook 'grep-mode-hook (lambda () (scf-mode 1)))

(defun w (&optional args)
  (interactive "p")
  (save-buffer args))

(defun q (&optional args)
  (interactive "P")
  (save-buffers-kill-emacs args))

(defun wq (&optional args)
  (interactive "P")
  (save-buffers-kill-emacs args))
