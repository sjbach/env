;;; -*- lexical-binding: t; -*-
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

(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tooltip-mode -1)

;; Show *Register Preview* quickly.
(setq register-preview-delay 0.25)  ;; default: 1

(require 'diminish)
(add-hook 'after-init-hook
          (lambda ()
            (diminish 'visual-line-mode)
            (diminish 'undo-tree-mode)
            (diminish 'company-mode)
            (diminish 'emacs-lock-mode)
            (diminish 'git-gutter-mode)
            (diminish 'eldoc-mode)
            (diminish 'elisp-slime-nav-mode)
            (diminish 'elisp-def-mode)
            (diminish 'flyspell-mode)
            (diminish 'which-key-mode)
            (diminish 'racer-mode)
            (diminish 'cargo-minor-mode)
            (diminish 'hs-minor-mode)))

;; Window splitter styling (for vertical splits)
(set-face-inverse-video 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "blue")  ;; magenta?
;; Set a nicer symbol than "|"
(require 'disp-table)  ;; ensure standard-display-table is set
(defun steve-set-smooth-window-divider ()
  (let ((display-table (or buffer-display-table
                           (window-display-table)
                           standard-display-table)))
    (when display-table
      (set-display-table-slot
       ;; Thinner alternative: ?│
       display-table 'vertical-border (make-glyph-code ?┃))
      (set-window-display-table (selected-window) display-table))))
(add-hook 'window-configuration-change-hook #'steve-set-smooth-window-divider)

;; Terminal title
(when (boundp 'xterm-set-window-title)
  (setq xterm-set-window-title t))

;; Consider:
;; (global-display-fill-column-indicator-mode 1)

;; ido remaps any key mapping to kill-buffer to map to ido-kill-buffer
;; instead. But this is overreaching- ido-kill-buffer is not a drop-in
;; replacement for all kill-buffer usages, so undo that.
(define-key (cdr ido-minor-mode-map-entry) [remap kill-buffer] nil)

(require 'git-gutter)
(setq git-gutter:modified-sign "*")  ;; overrides "="
(global-git-gutter-mode 1)

;; Faster eldoc annotations.
(setq eldoc-idle-delay 0.1)  ;; default: 0.5

;; More info in `undo-tree-visualize`.
(setq undo-tree-visualizer-relative-timestamps t)
(setq undo-tree-visualizer-timestamps t)

(require 'company)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-limit 30)
;; Lisp
;; A little extra highlighting in Lisp.
(require 'lisp-extra-font-lock)
(lisp-extra-font-lock-global-mode 1)
(require 'highlight-parentheses)
;;

(dumb-jump-mode 1)
;; (setq dumb-jump-selector 'helm)
(setq dumb-jump-selector 'ivy)

;; Ace window
(require 'ace-window)
;; In terminal Emacs you only view one frame at a time, so a global scope is
;; not usually what you want.
(setq aw-scope 'frame)  ;; vs global (all frames)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Which-key
(require 'which-key)
(setq which-key-allow-imprecise-window-fit t)
(setq which-key-sort-order 'which-key-prefix-then-key-order)
(setq which-key-allow-evil-operators t)
(setq which-key-idle-delay 0.25)  ;; When the connectivity is good
;; (setq which-key-idle-delay 1)  ;; When the connectivity is poor
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-is-verbose t)
(setq which-key-side-window-max-width 0.5)
(which-key-mode 1)
;; (which-key-setup-side-window-bottom)
(which-key-setup-side-window-right)

;; Mode line
;;
;; Show line and column numbers in the mode line.
(column-number-mode 1)
(line-number-mode 1)  ;; (in mode line, not in text body)
;;
;; Smart-mode-line
(require 'smart-mode-line)
(setq sml/show-frame-identification t)  ;; show frame name
(setq sml/theme 'smart-mode-line-powerline)
;; Other relevant variables:
;; - sml/name-width ;; default: 44
;; - sml/mode-width ;; default: 'full
;; - sml/replacer-regexp-list
(sml/setup)

;; In the terminal, change the cursor glyph based on Evil state.
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)  ;; warning: package not maintained
  ;; See `cursor-type` for options.  (Aside: 'hollow doesn't seem to work in
  ;; the terminal.)
  (setq evil-motion-state-cursor 'box)   ; █
  (setq evil-visual-state-cursor 'box)   ; █
  (setq evil-normal-state-cursor 'box)   ; █
  (setq evil-insert-state-cursor 'bar)   ; ⎸
  (setq evil-emacs-state-cursor  'hbar)  ; _
  ;; Note: there is a clear bug in `etcc--make-xterm-cursor-shape-seq' that can
  ;; produce this error:
  ;;
  ;; `Error in pre-command-hook (etcc--evil-set-cursor): (void-variable seq)'
  ;;
  ;; Re-running the activation function should resolve it.
  (evil-terminal-cursor-changer-activate))
;; (evil-terminal-cursor-changer-deactivate)

;; The completion buffer still shows too much boilerplate, but this helps
(setq completion-show-help nil)

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)
;; Prevent the annoying beep on errors
(setq visible-bell t)
;; Don't print "Saving file <filename>" on every save as that info is redundant
;; with the mode line indicator and these messages dominate the otherwise
;; helpful *Messages* buffer. (Aside: doesn't appear to be a simple way to
;; suppress "Wrote <filename>".)
(setq save-silently t)

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

