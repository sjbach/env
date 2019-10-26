(add-to-list 'load-path "~/.elisp")

(setq-default indent-tabs-mode nil)

(require 'lusty-explorer)

(global-set-key "\C-d" 'steve-close-buffer-and-window-unless-last)
(global-set-key (kbd "C-S-L") 'latex-preview-pane-mode)

;; Put all backups into a single directory
(setq backup-directory-alist `(("." . ,(expand-file-name "~/.emacs-tmp"))))
(setq-default vc-make-backup-files t)

;; Don't warn about opening symlinked files.
(setq-default vc-follow-symlinks t)

;; Use Spotlight for `locate` command on OS X
(when (string-equal system-type "darwin")
  (setq locate-command "mdfind"))

;; Save bookmarks to the fileysystem whenever there are changes.
(setq bookmark-save-flag 1)

;; Don't let *scratch* be closed accidentally.
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

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
   '(dash smart-mode-line solarized-theme fill-column-indicator undo-tree elisp-slime-nav ample-theme adaptive-wrap racer flycheck-rust company cargo))
 '(warning-suppress-types '((undo discard-info))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; ELPA:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Why: "In some circumstances, you may want to load packages explicitly in your
;; init file (usually because some other code in your init file depends on a
;; package). In that case, your init file should call the function
;; package-initialize."
(package-initialize)
(setq package-enable-at-startup nil) ; (because we just now did it.)

;; Posterity: installing new packages:
;; (list-packages) ; to update
;; Then: M-x package-install foo

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

