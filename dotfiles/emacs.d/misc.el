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
