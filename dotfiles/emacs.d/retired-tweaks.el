;; This file isn't loaded, it's just for posterity / searchability.
(assert false)

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
