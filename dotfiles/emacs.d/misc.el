
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
(defun steve--temp-paste-buffer-cleanup ()
  ;; Copy buffer contents to kill-ring / evil unnamed register.
  (when (string-equal (buffer-name) steve--temp-paste-buf-name)
    (widen)
    (kill-new
     (s-trim
      (buffer-string)))))
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
