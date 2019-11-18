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

;;
;; Adding new/custom words to the dictionary.
;; Stolen from: https://www.emacswiki.org/emacs/FlySpell
;;

(defun append-aspell-word (new-word)
 (let ((header "personal_ws-1.1")
       (file-name (substitute-in-file-name "$HOME/.aspell.en.pws"))
       (read-words (lambda (file-name)
                    (let ((all-lines (with-temp-buffer
                                      (insert-file-contents file-name)
                                      (split-string (buffer-string) "\n" t))))
                     (if (null all-lines)
                       ""
                      (split-string (mapconcat 'identity (cdr all-lines) "\n")
                                    nil
                                    t))))))
  (when (file-readable-p file-name)
   (let* ((cur-words (eval (list read-words file-name)))
          (all-words (delq header (cons new-word cur-words)))
          (words (delq nil (cl-remove-duplicates all-words :test 'string=))))
    (with-temp-file file-name
     (insert (concat header
                     " en "
                     (number-to-string (length words))
                     "\n"
                     (mapconcat 'identity (sort words #'string<) "\n"))))))
  (unless (file-readable-p file-name)
   (with-temp-file file-name
    (insert (concat header " en 1\n" new-word "\n")))))
 (ispell-kill-ispell t) ; restart ispell
 (flyspell-mode)
 (flyspell-mode))

(defun append-aspell-current ()
 "Add current word to aspell dictionary"
 (interactive)
 (append-aspell-word (thing-at-point 'word)))

