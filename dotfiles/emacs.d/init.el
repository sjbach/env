;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/elisp")

(setq-default indent-tabs-mode nil)

;; Custom input sequences.
;;
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
  (define-key input-map "\e[STEVE-S-DEL" (kbd "S-DEL"))
  (define-key input-map "\e[STEVE-C-DEL" (kbd "C-DEL"))
  (define-key input-map "\e[STEVE-C-S-DEL" (kbd "C-S-DEL"))
  (define-key input-map "\e[STEVE-C-/" (kbd "C-/"))
  ;; STEVE Not yet set in iTerm2 b/c it is a macOS binding.
  (define-key input-map "\e[STEVE-C-TAB" (kbd "C-TAB"))
  )

;; Context-free global bindings.
;;
;; Instead of kmacro-start-macro-or-insert-counter
(global-set-key [f3] 'steve-juggle-previous-buffer)
;; Instead of kmacro-end-or-call-macro
(global-set-key [f4] #'other-window)
(global-set-key [S-f4] (kbd "C-u - <f4>"))
(global-set-key [f5] #'other-frame)
(global-set-key [S-f5] (kbd "C-u - <f5>"))
;;
(global-set-key [C-return] 'steve-juggle-previous-buffer)
;; Aside: `DEL` refers to the backspace key; The Delete key is
;; `delete`/`<deletechar>`.
(global-set-key (kbd "C-DEL") #'steve-kill-buffer)
(global-set-key (kbd "C-S-DEL") #'kill-buffer-and-window)
;;
;; I use frames as pseudo-workspaces, I don't want to delete them all
;; accidentally because of a mistype.
(define-key ctl-x-5-map "1"
  (lambda (&optional dummy)
    (interactive)
    (message "Run `M-x delete-other-frames'")))
;;
;; Change the active window with Shift-<arrow key>.
(require 'windmove)
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; Backups and auto-saves.
;; Put all backups into a single directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq-default vc-make-backup-files t)
;; Don't create backups by renaming, create by copying. A little slower I guess
;; but I bet rarely significant.
(setq backup-by-copying t)
;;
;; This is just the default value. On one of my systems it's overridden during
;; startup to a relative path, not sure why, so re-appply it here.
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")
;; Auto save files all in one directory rather than dropped wherever I'm
;; editing.
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))

;; Don't warn about opening symlinked files.
(setq vc-follow-symlinks t)
;; Don't ask to save buffers before running a grep.
(setq grep-save-buffers nil)

;; Use Spotlight for `locate` command on OS X
(when (string-equal system-type "darwin")
  (setq locate-command "mdfind"))

;; Preserve minibuffer history.
(savehist-mode 1)
; Preserve cursor location when a file is reopened.
(save-place-mode 1)
;; Save bookmarks to the fileysystem whenever there are changes.
(setq bookmark-save-flag 1)
(setq messages-buffer-max-lines 32768)  ;; arbitrary high number
;; Could be higher, but this is the limit used by desktop-save-mode and I don't
;; see much value in preserving >200 open buffers.
(setq history-length 200)

(when (file-exists-p "~/emacs-src-git/src/")
  (setq find-function-C-source-directory
        "~/emacs-src-git/src/"))

;; Persistent scratch buffer.
(require 'remember)
(setq remember-notes-buffer-name "*Scratch*")
(setq initial-buffer-choice
      (lambda ()
        (prog1
            (remember-notes)
          ;; Don't let the notes buffer be closed accidentally.
          (with-current-buffer remember-notes-buffer-name
            (emacs-lock-mode 'kill)))))

;; Custom/customize stuff is auto-edited by Emacs and superstition
;; pressures me to have that done in its own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'lusty-explorer)

(require 'magit)
(setq magit-diff-refine-hunk 'all)
;; Can be slow in large repos.
(add-hook 'after-save-hook #'magit-after-save-refresh-status)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq git-timemachine-show-author nil)

;; `xterm-paste` pushes what it pastes onto the kill ring, which overwrites
;; Evil's nameless register and shifts all the numbered registers down. I
;; understand why it happens but it's not useful to me, so undo it.
(defun steve--delete-top-of-kill-ring (&rest d)
  (let ((interprogram-paste nil)
        (yank-pop-change-selection nil)
        (interprogram-cut-function nil))
    (current-kill 1 nil))
  nil)
(advice-add #'xterm-paste :after #'steve--delete-top-of-kill-ring)

;; Package
;;
(require 'package)
;; ELPA, MELPA:
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defun expand-load (filename)
  (when (stringp filename)
    (load-file (expand-file-name (format "~/.emacs.d/%s" filename)))))

(mapc 'expand-load
      `("look-and-feel.el"
        "prose.el"
        "misc.el"
        "code-conventions.el"
        "clojure.el"
        "R.el"
        "rust.el"
        "steve-evil.el"
        ,(and (file-exists-p "~/.emacs.d/nonpublic.el") "nonpublic.el")
        ))

;; Launch Emacs server.
(require 'server)
(unless (server-running-p)
  (server-start))

;; Restore previous session's frameset, buffers, modes, etc.
(desktop-save-mode 1)
(setq desktop-globals-to-save
      (append desktop-globals-to-save
              '((extended-command-history . 50)
                (regexp-history . 50))))
;;
;; desktop-save-mode will not restore frames in terminal Emacs because of a
;; check for `(display-graphic-p)` in `(desktop-restoring-frameset-p)`. But the
;; restore works just fine if you force it.
;;
;; Inspiration: https://emacs.stackexchange.com/a/45829
(unless (display-graphic-p)
  (setq desktop-restore-forces-onscreen nil)
  (defun always-t () t)
  ;; Note: this only works when called by the hook below.
  (defun steve--restore-desktop-frameset-even-in-tty ()
    (advice-add #'display-graphic-p :override #'always-t)
    (desktop-restore-frameset)
    (advice-remove #'display-graphic-p #'always-t)
    (fmakunbound #'always-t))
  (add-hook 'desktop-after-read-hook
            #'steve--restore-desktop-frameset-even-in-tty))
