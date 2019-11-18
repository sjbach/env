;;
;; Look and feel
;;

;; Scrolling behavior. (Make similar to Vim)
;;
;; Begin scrolling the window four lines before the margin.
(setq scroll-margin 4)
;; When point moves out of the window, don't recenter the window on point,
;; rather scroll just enough to get point in the window again (respecting the
;; margin above).
(setq scroll-conservatively 200)  ; (arbitrary high number)

;; Different color for parens
(require 'paren-face)
(global-paren-face-mode 1)

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tooltip-mode -1)

(require 'git-gutter)
(global-git-gutter-mode 1)
(require 'company)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 30)
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

;; Show colums and lines in the status bar
(column-number-mode t)
(line-number-mode 1)

;; Unexpected new windows are annoying.
(setq pop-up-windows nil)

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
  (font-lock-add-keywords 'sh-mode words)
  (font-lock-add-keywords 'rust-mode words))

(when (string-equal system-type "darwin")
  (toggle-frame-maximized))

