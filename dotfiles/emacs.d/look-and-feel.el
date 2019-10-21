;;
;; Look and feel
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

;; Scrolling behavior.
;;
;; Begin scrolling the window three lines before the margin.
(setq scroll-margin 3)
;; When point moves out of the window, don't recenter the window on point,
;; rather scroll just enough to get point in the window again (respecting the
;; margin above).
(setq scroll-conservatively 200)  ; (arbitrary high number)

; Slime autodoc used to expand the minibuffer height, which was annoying.
;(setq resize-mini-windows nil)

;; Different color for parens
(require 'parenface)

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq inhibit-startup-message t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;(set-scroll-bar-mode 'right)
(toggle-scroll-bar -1)

;; force horizontal splits - stolen from stackoverflow somewhere
(setq split-height-threshold
      (if (>= emacs-major-version 23)
        nil
        999))
(setq split-width-threshold 0)

;; Try to prevent lots of window splitting.  Stolen with modification from here
;; http://stackoverflow.com/questions/1381794/too-many-split-screens-opening-in
;;
(defun steve-display-buffer-fn (buffer current-window-unacceptable-p)
  (if (and (not pop-up-frames)
           (one-window-p)
           (or current-window-unacceptable-p
               (not (eq (window-buffer (selected-window))
                        buffer)))
;           (> (frame-width) 162)
           )
      (split-window-horizontally))
  ;; Note: Some modules sets `pop-up-windows' to t before calling
  ;; `display-buffer'
  (let ((display-buffer-function nil)
        (pop-up-windows nil))
;    (display-buffer buffer current-window-unacceptable-p)))
    (display-buffer buffer nil)))
(setq display-buffer-function 'steve-display-buffer-fn)

;; Shorten paths in grep-mode
;; FIXME can I do this quicker so that the pre-overlay names aren't visible?
;(require 'scf-mode)
;(add-hook 'grep-mode-hook (lambda () (scf-mode 1)))
;; NOTE disabled now that I'm not working in java as much

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

;; Turn on font-lock mode for syntax highlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Show colums and lines in the status bar
(column-number-mode t)
(line-number-mode 1)

;; New windows are annoying.
(setq pop-up-windows nil)

;; Enable visual feedback on selections
(transient-mark-mode t)

;; The completion buffer still shows too much boilerplate, but this helps
(setq completion-show-help nil)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; Prevent the annoying beep on errors
(setq visible-bell t)

;; Highlight XXX style code tags in source
(let ((words
       '(("\\<\\(FIXME\\|HACK\\|XXX\\|TODO\\|BUG\\|STEVE\\)"
          1 font-lock-warning-face prepend))))
  (font-lock-add-keywords 'c-mode words)
  (font-lock-add-keywords 'c++-mode words)
  (font-lock-add-keywords 'emacs-lisp-mode words)
  (font-lock-add-keywords 'ess-mode words)
  (font-lock-add-keywords 'html-mode words)
  (font-lock-add-keywords 'java-mode words)
  (font-lock-add-keywords 'js-mode words)
  (font-lock-add-keywords 'latex-mode words)
  (font-lock-add-keywords 'lisp-mode words)
  (font-lock-add-keywords 'nxml-mode words)
  (font-lock-add-keywords 'python-mode words)
  (font-lock-add-keywords 'ruby-mode words)
  (font-lock-add-keywords 'sh-mode words))

(when (string-equal system-type "darwin")
  (toggle-frame-maximized))

