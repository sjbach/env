;; This file isn't loaded, it's just for posterity / searchability.
(assert false)

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
