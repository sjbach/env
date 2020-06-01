;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)

;; This instead of `comment-dwim' because I prefer the behavior of commenting
;; out the current line, if region is inactive, rather than adding an
;; annotation comment to the current line.
(defun steve-comment-line-or-region-dwim ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning)
                                   (region-end)
                                   nil)
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position)
                                 nil)))

;; Copy above point the current line, or the lines covered by the region, and
;; then comment out the copy.
(defun steve-duplicate-and-comment-out ()
  (interactive)
  (cl-multiple-value-bind (start-pos end-pos)
      (if (use-region-p)
          (list (save-excursion
                  (goto-char (region-beginning))
                  (line-beginning-position))
                (save-excursion
                  (goto-char (if (eq (char-before) ?\n)
                                 (1- (region-end))
                               (region-end)))
                  (line-end-position)))
        (list (line-beginning-position)
              (line-end-position)))
    (cl-assert start-pos)
    (cl-assert end-pos)
    (save-excursion
      (goto-char start-pos)
      (let ((region-str (buffer-substring start-pos end-pos)))
        (insert region-str "\n"))
      ;; (comment-or-uncomment-region start-pos
      (comment-region start-pos (point) nil))))

;; Simpler wrapper on `kill-buffer' that does not prompt for a buffer name.
(defun steve-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun steve-pp-eval-dwim ()
  (interactive)
  (let ((sexp
         (cond ((use-region-p)
                ;; Eval region.
                (read
                 (buffer-substring-no-properties (region-beginning)
                                                 (region-end))))
               ;; Aside: in Evil w/ filled cursor, char-after is the character under
               ;; cursor and char-before is the character before cursor.
               ((eq (char-after) ?\ )  ;; if Evil cursor is on a space
                ;; Eval surrounding sexp.
                (save-excursion
                  (up-list)
                  (sexp-at-point)))
               ((eq (char-after) ?\()  ;; if Evil cursor is on a left paren
                ;; Eval next sexp (i.e. that begins with this parenthesis).
                (sexp-at-point))
               ((eq (char-after) ?\))  ;; if Evil cursor is on a right paren
                ;; Eval prior (enclosing) sexp, including this paren
                (save-excursion
                  (up-list)
                  (sexp-at-point)))
               (t
                (sexp-at-point)))))
    (message "Evaluating: `%s'" sexp)
    (pp-eval-expression sexp)))

; STEVE make work in terminal
;; (defun steve-vim-excursion ()
;;   ;; FIXME cleanup
;;   (interactive)
;;   (let ((display (getenv "DISPLAY")))
;;     (if (and display (> (length display) 0))
;;       (let ((file (buffer-file-name)))
;;         (cond ((null file) (message "Buffer not visiting a file"))
;;               ((buffer-modified-p) (message "Buffer is modified!"))
;;               (t
;;                 (call-process "gvim" nil nil nil file)
;;                 (ex-edit))))
;;       (user-error "No DISPLAY available."))))

;; Command to create a temporary buffer in which to paste and clean up text
;; without interference by the particular major mode.
(defvar steve--temp-paste-buf-name
  ;; Posterity: leading space means hidden.
  " *Steve text paste buffer*")
;;
(defun steve-text-pasting-excursion ()
  (interactive)
  (let ((buffer-to-return-to (current-buffer)))
    ;; Kill the paste buffer if it exists.
    (let ((old-temp-paste-buf (get-buffer steve--temp-paste-buf-name))
          (kill-buffer-hook nil))  ; don't run hooks.
      (when old-temp-paste-buf
        (kill-buffer old-temp-paste-buf)))
    (pop-to-buffer (get-buffer-create steve--temp-paste-buf-name))
    (cl-assert (eq (current-buffer)
                   (get-buffer-create steve--temp-paste-buf-name)))
    ;; At close, copy buffer contents to kill-ring / evil unnamed register.
    (add-hook 'kill-buffer-hook
              (lambda ()
                ;; (Should always be the case.)
                (when (string-equal (buffer-name) steve--temp-paste-buf-name)
                  (widen)
                  (kill-new
                   (s-trim
                    (buffer-string)))
                  (pop-to-buffer buffer-to-return-to)))
              nil
              'local))
  ;; Disable auto-indent.
  (electric-indent-local-mode -1)
  (evil-insert-state)
  (message "(Now in insert mode)"))

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
  ;; "True" previous buffer; forgo the logic of preferring non-visible buffers.
  (switch-to-buffer (other-buffer 'visible-ok) nil 'force-same-window))

(defun steve-show-help-buffer ()
  (interactive)
  (let ((help-buffer-name (help-buffer)))
    (if (null help-buffer-name)
        (error "No help buffer.")
      (let ((help-window (get-buffer-window help-buffer-name)))
        (if help-window
            (select-window help-window)
          ;; Prefer the buffer be shown in a different window.
          ;; (pop-to-buffer help-buffer-name))))))
          (save-selected-window
            (switch-to-buffer-other-window help-buffer-name)))))))

(defun steve-jump-to-scratch ()
  (interactive)
  (unless (string= (ef--frame-name) "research")
    (ef-frame-choose "research"))
  (pop-to-buffer "*Scratch*"))

(defun steve-byte-compile-and-load-current-file ()
  (interactive)
  (let ((elisp-file-name (buffer-file-name (current-buffer))))
    (unless (string-match-p "\.el$" elisp-file-name)
      (error "Does not look like an elisp file: %s" elisp-file-name))
    (byte-compile-file elisp-file-name 'load)))

;;;
;;; Debugging
;;;

(defun steve-list-core-font-faces ()
  (interactive)
  (list-faces-display
   (rx string-start
       (or "default" "bold" "italic" "bold-italic" "underline"
           "fixed-pitch" "fixed-pitch-serif")
       string-end)))

(defun steve-list-predefined-font-lock-faces ()
  (interactive)
  (list-faces-display
   (rx string-start "font-lock-" (* not-newline) "-face" string-end)))

(defun steve-list-active-minor-modes (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (let ((all-minor-modes
         (with-current-buffer buffer
           (-uniq
            (sort
             (-filter
              (lambda (sym) (and (boundp sym) (symbol-value sym)))
              (append
               (mapcar #'car minor-mode-alist)
               (cl-copy-list minor-mode-list)))
             (lambda (a b)
               (string< a b)))))))
    (with-temp-buffer-window "*steve-minor-modes*" nil nil
      (with-current-buffer "*steve-minor-modes*"
        (insert
         (string-join (mapcar #'symbol-name all-minor-modes)
                      "\n"))))))

(defmacro STEVE-dp (&rest args)
  "Debug print: evaluate the given form (just once, in case it has
side-effects), print its representation to *Messages*, and return it.

ARGS is a single form or an annotation string and a form."
  (let ((sym-var (gensym "STEVE-name"))
        (val-var (gensym "STEVE-val")))
    (if (and (= (length args) 1)
             (stringp (car args)))
        `(message ,(concat "STEVE-dp \"" (car args) "\""))
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
           ,val-var)))))

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

