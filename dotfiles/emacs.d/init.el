(add-to-list 'load-path "~/.elisp")

(setq-default indent-tabs-mode nil)

(require 'lusty-explorer)

(global-set-key "\C-d" 'steve-close-buffer-and-window-unless-last)
(global-set-key (kbd "C-S-L") 'latex-preview-pane-mode)

;; Put all backups into a single directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs.d/backup"))))
(setq-default vc-make-backup-files t)

;; Don't warn about opening symlinked files.
(setq-default vc-follow-symlinks t)

;; Use Spotlight for `locate` command on OS X
(when (string-equal system-type "darwin")
  (setq locate-command "mdfind"))

;; Preserve minibuffer history.
(savehist-mode 1)
; Preserve cursor location when a file is reopened.
(save-place-mode 1)
;; Save bookmarks to the fileysystem whenever there are changes.
(setq bookmark-save-flag 1)

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
(require 'remember)
(setq remember-notes-buffer-name "*scratch*")
;; (setq initial-buffer-choice #'remember-notes)
(setq initial-buffer-choice
      (lambda ()
        (kill-buffer remember-notes-buffer-name)  ; *scratch*
        (prog1
            (remember-notes)
          ;; Don't let *scratch* be closed accidentally.
          (with-current-buffer "*scratch*"
            (emacs-lock-mode 'kill)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-collection-setup-minibuffer t)
 '(evil-want-keybinding nil)
 '(evil-want-minibuffer t)
 '(js-indent-level 2)
 '(load-home-init-file t t)
 '(package-selected-packages
   '(paren-face evil-magit magit dash smart-mode-line solarized-theme fill-column-indicator undo-tree elisp-slime-nav ample-theme adaptive-wrap racer flycheck-rust company cargo))
 '(warning-suppress-types '((undo discard-info))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
        "code-conventions.el"
        "misc.el"
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

