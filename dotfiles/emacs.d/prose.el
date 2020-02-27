;;; -*- lexical-binding: t -*-

(require 'flyspell)
;;
;; Don't print noisy progress to *Messages*.
(setq flyspell-issue-message-flag nil)
;;
;; Make `flyspell-correct-word-before-point' work in the terminal; a textual
;; popup.
;; Source: https://www.emacswiki.org/emacs/FlySpell
(defun steve-flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
                       (sort (car (cdr (cdr poss))) 'string<)
                     (car (cdr (cdr poss)))))
         (cor-menu (if (consp corrects)
                       (mapcar (lambda (correct)
                                 (list correct correct))
                               corrects)
                     '()))
         (affix (car (cdr (cdr (cdr poss)))))
         show-affix-info
         (base-menu  (let ((save (if (and (consp affix) show-affix-info)
                                     (list
                                      (list (concat "Save affix: " (car affix))
                                            'save)
                                      '("Accept (session)" session)
                                      '("Accept (buffer)" buffer))
                                   '(("Save word" save)
                                     ("Accept (session)" session)
                                     ("Accept (buffer)" buffer)))))
                       (if (consp cor-menu)
                           (append cor-menu (cons "" save))
                         save)))
         (menu (mapcar
                (lambda (arg) (if (consp arg) (car arg) arg))
                base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
(fset 'flyspell-emacs-popup 'steve-flyspell-emacs-popup-textual)

;; Using proselint in text.
;;
;; Cribbed from:
;; https://github.com/amperser/proselint/blob/master/plugins/flycheck/flycheck-proselint.el
;;
;; Posterity: installation instructions:
;; - Install flycheck using package / MELPA;
;; - `pip install proselint`;
;; - Figure out where the proselint executable went and place it on the path.
;;
(require 'flycheck)
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
(add-to-list 'flycheck-checkers 'proselint)
(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)

;; Adding new/custom words to the dictionary.
;; Source: https://www.emacswiki.org/emacs/FlySpell
;;
(defun steve-append-aspell-word (new-word)
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
 (flyspell-mode))
;;
(defun steve-append-aspell-current ()
 "Add current word to aspell dictionary"
 (interactive)
 (steve-append-aspell-word (thing-at-point 'word)))

