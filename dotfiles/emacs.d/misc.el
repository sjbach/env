;;; -*- lexical-binding: t; -*-

(defun steve-comment-line-or-region ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning)
                                   (region-end)
                                   nil)
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position)
                                 nil)))

(defun steve-turn-on-fill-column-indiciator ()
  (if (boundp 'display-fill-column-indicator)
      ;; Only present in 27.1+.
      (display-fill-column-indicator-mode 1)
    (turn-on-fci-mode)))

;; Simpler wrapper on `kill-buffer` that does not prompt for a buffer name.
(defun steve-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(defun steve-show-macroexpansion-for-region (beg end)
  (interactive "r")
  (unless (and beg end)
    (error "No region given"))
  (let* ((s (buffer-substring-no-properties beg end))
         (sexp (read s))
         (macroexpanded (macroexpand-1 sexp))
         (buf-name "*Steve-Macroexpanded*")
         (temp-buffer-setup-hook '(emacs-lisp-mode)))
    (with-output-to-temp-buffer buf-name
      ;(print macroexpanded)
      (pp macroexpanded))))

; STEVE make work in terminal
(defun steve-vim-excursion ()
  ;; FIXME cleanup
  (interactive)
  (let ((display (getenv "DISPLAY")))
    (if (and display (> (length display) 0))
      (let ((file (buffer-file-name)))
        (cond ((null file) (message "Buffer not visiting a file"))
              ((buffer-modified-p) (message "Buffer is modified!"))
              (t
                (call-process "gvim" nil nil nil file)
                (ex-edit))))
      (user-error "No DISPLAY available."))))

(defvar steve--temp-paste-buf-name
  ;; Posterity: leading space means hidden.
  " *Steve text paste buffer*")
;;
(defun steve--temp-paste-buffer-cleanup ()
  ;; Copy buffer contents to kill-ring / evil unnamed register.
  (when (string-equal (buffer-name) steve--temp-paste-buf-name)
    (widen)
    (kill-new
     (s-trim
      (buffer-string)))))
;;
(defun steve-text-pasting-excursion ()
  (interactive)
  ;; Kill the paste buffer if it exists.
  (let ((old-temp-paste-buf (get-buffer steve--temp-paste-buf-name))
        (kill-buffer-hook nil))  ; don't run hooks.
    (when old-temp-paste-buf
      (kill-buffer old-temp-paste-buf)))
  (let ((temp-paste-buf (get-buffer-create steve--temp-paste-buf-name)))
    (pop-to-buffer temp-paste-buf)
    ;; Disable auto-indent.
    (electric-indent-local-mode -1)
    ;(use-local-map (copy-keymap foo-mode-map))
    ;(local-set-key "d" 'some-function)
    (add-hook 'kill-buffer-hook
              #'steve--temp-paste-buffer-cleanup)
    (evil-insert-state)
    (message "(Now in insert mode)")))

(defun steve-remove-matching-lines ()
  (interactive)
  (let ((regexp (read-regexp "Remove matching")))
    (unwind-protect
        (progn
          (setq buffer-read-only nil)
          (flush-lines regexp))
      (setq buffer-read-only t))))

(defun steve-juggle-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))


;; Debug print. Evaluate the given form (just once, in case it has
;; side-effects), print its representation to *Messages*, and return it.
(defmacro STEVE-dp (arg)
  (let ((name-var (gensym "STEVE-name"))
        (val-var (gensym "STEVE-val")))
    `(let ((,name-var ',arg)
           (,val-var ,arg))
       (message "STEVE-dp %S: %S" ,name-var ,val-var)
       (message nil)
       ,val-var)))

(defun steve-toggle-dp-on-sexp ()
  (interactive)
  (save-mark-and-excursion
    (skip-chars-forward "[:space:]\n")
    (cl-destructuring-bind (sexp-beg . sexp-end)
        (bounds-of-thing-at-point 'sexp)
      (cl-assert (and sexp-beg sexp-end))
      (save-restriction
        (goto-char sexp-beg)
        (narrow-to-region sexp-beg sexp-end)
        (check-parens)  ;; (Just in case; should not be able to fail here.)
        (atomic-change-group
          (if (looking-at (rx point "(STEVE-dp"))
              ;; Remove the (STEVE-dp ...) wrapper.
              (progn
                (delete-char (length "(STEVE-dp"))
                (delete-horizontal-space)
                (end-of-buffer)
                (backward-char)
                (cl-assert (looking-at ")"))
                (delete-char 1))
            ;; Apply the (STEVE-dp ...) wrapper.
            (insert "(STEVE-dp ")
            (end-of-buffer)
            (insert ")")))))))
