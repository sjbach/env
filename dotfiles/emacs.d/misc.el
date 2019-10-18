
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

(defun steve-remove-matching-lines ()
  (interactive)
  (let ((regexp (read-regexp "filter")))
    (unwind-protect
        (progn
          (setq buffer-read-only nil)
          (flush-lines regexp))
      (setq buffer-read-only t))))

(defun steve-juggle-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun steve-close-buffer-and-window-unless-last ()
  (interactive)
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (next (next-window window)))
    (kill-buffer buffer)
    (when (and window
               (not (eq window next)))
      (delete-window window))))

;; Debug print
(defmacro STEVE-dp (arg)
  `(let ((STEVE-name ',arg)
         (STEVE-val ,arg))
     (message "STEVE-dp %S: %S" STEVE-name STEVE-val)
     STEVE-val))
