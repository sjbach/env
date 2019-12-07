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

(defun steve-show-help-buffer ()
  (interactive)
  (let ((help-buffer-name (help-buffer)))
    (if (null help-buffer-name)
        (error "No help buffer.")
      (let ((help-window (get-buffer-window help-buffer-name)))
        (if help-window
            (select-window help-window)
          (pop-to-buffer help-buffer-name))))))

;; Debug print. Evaluate the given form (just once, in case it has
;; side-effects), print its representation to *Messages*, and return it.
(require 'cl-lib)
(defmacro STEVE-dp (&rest args)
  (let ((sym-var (gensym "STEVE-name"))
        (val-var (gensym "STEVE-val")))
    (cl-multiple-value-bind (msg-string sexp)
        (cl-case (length args)
          (0 (error "STEVE-dp: no args provided"))
          (1 (cl-values "STEVE-dp %S: %S" (car args)))
          (2 (unless (stringp (car args))
               (error "STEVE-dp: malformed args: %S" args))
             (let ((annotation (car args)))
               (cl-values (format "STEVE-dp \"%s\" %%S: %%S" annotation)
                          (cadr args))))
          (t (error "STEVE-dp: too many args: %S" args)))
      `(let ((,sym-var ',sexp)
             (,val-var ,sexp))
         (message ,msg-string ,sym-var ,val-var)
         (message nil)
         ,val-var))))

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
              (let ((has-annotation-p
                     (let ((sexp (read (buffer-string))))
                       (cl-case (length sexp)
                         ((0 1) (error "Form is empty"))
                         ((2 3)
                          (unless (eq (car sexp) 'STEVE-dp)
                            (error "Not a debug-print form"))
                          (if (= (length sexp) 3)
                              (progn
                                (when (not (stringp (cadr sexp)))
                                  (error "Form is invalid"))
                                t)
                            nil))
                         (t (error "Form is invalid"))))))
                ;; Remove the (STEVE-dp ...) wrapper.
                (delete-char (length "(STEVE-dp"))
                (delete-horizontal-space)
                (when has-annotation-p
                  (cl-assert (looking-at (rx point "\"")))
                  ;; There is an annotation string - remove it as well.
                  (cl-destructuring-bind (str-beg . str-end)
                      (bounds-of-thing-at-point 'sexp)
                    (delete-region str-beg str-end))
                  (delete-horizontal-space))
                ;; Trailing ")".
                (end-of-buffer)
                (backward-char)
                (cl-assert (looking-at ")"))
                (delete-char 1))
            ;; Apply the (STEVE-dp ...) wrapper.
            (insert "(STEVE-dp ")
            (end-of-buffer)
            (insert ")")))))))
