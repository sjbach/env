;; Posterity: installation instructions:
;; - Install flycheck using package / MELPA;
;; - `pip install proselint`;
;; - Figure out where the proselint executable went and place it on the path.

;; Flycheck isn't loaded until after package-initialize; my impression is that
;; preferred practice is to let Emacs run package-initialize, which means we have
;; to defer the stuff below until that has been done.
;;
(add-hook 'after-init-hook 'defer-flycheck-stuff-hook)
(defun defer-flycheck-stuff-hook ()

  (require 'flycheck)

  ;; Cribbed from:
  ;; https://github.com/amperser/proselint/blob/master/plugins/flycheck/flycheck-proselint.el

  (add-hook 'markdown-mode-hook #'flycheck-mode)
  (add-hook 'text-mode-hook #'flycheck-mode)

  (flycheck-define-checker proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
              (id (one-or-more (not (any " "))))
              (message (one-or-more not-newline)
                       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
              line-end))
    :modes (text-mode markdown-mode gfm-mode))

  (add-to-list 'flycheck-checkers 'proselint))
