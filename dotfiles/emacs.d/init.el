;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/elisp")

(setq-default indent-tabs-mode nil)

;; Backups and auto-saves.
;; Put all backups into a single directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq tramp-backup-directory-alist backup-directory-alist)
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
(make-directory "~/.emacs.d/auto-saves/" 'already-exists-ok)
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
(setq history-length 200)  ;; default: 100
(setq kill-ring-max 200) ;; default: 60

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
        "R.el"
        "rust.el"
        "keys.el"
        "evil-tweaks.el"
        ,(and (file-exists-p "~/.emacs.d/nonpublic.el") "nonpublic.el")
        ))

;; Launch Emacs server.
(require 'server)
(unless (server-running-p)
  (server-start))

;;;
;;; Desktop-save-mode
;;;

;; Restore previous session's frameset, buffers, modes, etc.
(desktop-save-mode 1)
(setq desktop-globals-to-save
      (append desktop-globals-to-save
              '((extended-command-history . 50)
                (regexp-history . 50)
                cf--name-to-numbered-confs-alist
                cf--name-to-pre-restore-conf-alist)))

;; I want desktop-save-mode to save/restore frame names.
;; Note: might be better to do this as advice on desktop-save and desktop-read.
(require 'frameset)  ;; ensure `frameset-filter-alist` is loaded
(unless (eq (alist-get 'name frameset-filter-alist) nil)
  (push '(name . nil) frameset-filter-alist))
;; Never save the powerline cache as it's known to cause problems upon restore.
(unless (eq (alist-get 'powerline-cache frameset-filter-alist) :never)
  (push '(powerline-cache . :never) frameset-filter-alist))

;; desktop-save-mode will not restore frames in terminal Emacs because of a
;; check for `(display-graphic-p)` in `(desktop-restoring-frameset-p)`. But the
;; restore works just fine if you force it.
;;
;; Inspiration: https://emacs.stackexchange.com/a/45829
(unless (display-graphic-p)
  ;; (setq desktop-restore-forces-onscreen 'all)  ;; causes error in tty
  (setq desktop-restore-forces-onscreen nil)
  (defun steve-always-t () t)
  ;; Note: this only works when called by the hook below.
  (defun steve--restore-desktop-frameset-even-in-tty ()
    (advice-add #'display-graphic-p :override #'steve-always-t)
    (desktop-restore-frameset)
    (advice-remove #'display-graphic-p #'steve-always-t)
    (message "Frames restored"))
  (add-hook 'desktop-after-read-hook
            #'steve--restore-desktop-frameset-even-in-tty))

;; Note for posterity: `package-initialize', if not called prior to this point,
;; runs at this point (effectively), after which the functions on
;; `after-init-hook' run.
