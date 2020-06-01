;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/elisp")

;; No tab characters in new indentation.
(setq-default indent-tabs-mode nil)
;; Fill doesn't need two spaces after a period.
(setq-default sentence-end-double-space nil)

;; Backups and auto-saves.
;;
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

;; Prefer the filename found by evaluating all symbolic links.
(setq find-file-visit-truename t)
;; Suppress "foo and bar are the same file" warnings. The above setting and/or
;; `find-file-existing-other-name' ensure we dereference symlinks and keep a
;; canonical buffer, so the warning is not helpful.
(setq find-file-suppress-same-file-warnings t)

;; Don't load old byte code if the source .el file has a newer timestamp.
;; (Posterity: once caused a Recursive Load error re: jka-compr.el.gz but seems
;; to be okay now)
(setq load-prefer-newer t)

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
;; These could be higher, but this is the limit used by desktop-save-mode and I
;; don't see much value in preserving >200 open buffers.
(setq history-length 200)  ;; default: 100
(setq kill-ring-max 200) ;; default: 60
(setq list-command-history-max 200)  ;; default: 32

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

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)  ;; default: 20

(require 'magit)
(setq magit-diff-refine-hunk 'all)
;; Refresh magit status upon save. Can be slow in large repos.
;; Disabled: appears to sometimes cause `point' to jump to a different faraway
;; position in the saved buffer.
;; (add-hook 'after-save-hook #'magit-after-save-refresh-status)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq git-timemachine-show-author nil)

;; hideshow seems to sometimes overreach in folding comments and can suppress
;; a block of code if it sits between two large comment blocks.
(setq hs-hide-comments-when-hiding-all nil)

;; `xterm-paste', activated when terminal Emacs detects that input is a pasted
;; text stream, pushes what it pastes onto the kill ring, which overwrites
;; Evil's nameless register and shifts all the numbered registers down by 1. I
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
        "misc-functions.el"
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
                ef--name-to-numbered-confs-alist
                ef--name-to-pre-restore-conf-alist)))

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
    (unwind-protect
        (progn
          (desktop-restore-frameset)
          (message "Frames restored"))
      (advice-remove #'display-graphic-p #'steve-always-t)))
  (add-hook 'desktop-after-read-hook
            #'steve--restore-desktop-frameset-even-in-tty))

;; Note for posterity: `package-initialize', if not called prior to this point,
;; runs at this point (effectively), after which the functions on
;; `after-init-hook' run.
