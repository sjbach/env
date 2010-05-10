;;; lusty-explorer.el --- Dynamic filesystem explorer and buffer switcher
;;
;; Copyright (C) 2008-2010 Stephen Bach <this-file@sjbach.com>
;;
;; Version: 2.2
;; Created: May 9, 2010
;; Keywords: convenience, files, matching
;; Compatibility: GNU Emacs 22 and 23
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; To install, copy this file somewhere in your load-path and add this line to
;; your .emacs:
;;
;;    (require 'lusty-explorer)
;;
;; To launch the explorer, run or bind the following commands:
;;
;;    M-x lusty-file-explorer
;;    M-x lusty-buffer-explorer
;;
;; And then use as you would `find-file' or `switch-to-buffer'. A split window
;; shows the *Lusty-Matches* buffer, which updates dynamically as you type
;; using a fuzzy matching algorithm.  One match is highlighted; you can move
;; the highlight using C-n / C-p (next, previous) and C-f / C-b (next column,
;; previous column).  Pressing TAB or RET will select the highlighted match.
;;
;; To create a new buffer with the given name, press C-x e.  To open dired at
;; the current viewed directory, press C-x d.
;;
;; Note: lusty-explorer.el benefits greatly from byte-compilation.  To byte-
;; compile this library, M-x byte-compile-file and choose lusty-explorer.el.
;; (Ignore any warnings about the cl package.) Then, restart Emacs or
;; M-x load-library and choose the newly generated lusty-explorer.elc file.
;;
;;; Customization:
;;  --------------
;;
;; To modify the keybindings, use something like:
;;
;;   (add-hook 'lusty-setup-hook 'my-lusty-hook)
;;   (defun my-lusty-hook ()
;;     (define-key lusty-mode-map "\C-j" 'lusty-highlight-next))
;;
;; Respects these variables:
;;   completion-ignored-extensions
;;
;; Latest release: <http://www.emacswiki.org/cgi-bin/wiki/LustyExplorer>
;; Development:    <http://github.com/sjbach/lusty/tree/master>
;;

;;; Contributors:
;;
;; Jan Rehders
;; Hugo Schmitt
;; Volkan Yazici
;; René Kyllingstad
;;

;;; Code:

;; Used for many functions and macros.
(require 'cl)

;; Used only for its faces (for color-theme).
(require 'font-lock)

(declaim (optimize (speed 3) (safety 0)))

(defgroup lusty-explorer nil
  "Quickly open new files or switch among open buffers."
  :group 'extensions
  :group 'convenience
  :version "23")

(defcustom lusty-setup-hook nil
  "Hook run after the lusty keymap has been setup.
Additional keys can be defined in `lusty-mode-map'."
  :type 'hook
  :group 'lusty-explorer)

(defvar lusty-match-face font-lock-function-name-face)
(defvar lusty-directory-face font-lock-type-face)
(defvar lusty-slash-face font-lock-keyword-face)
(defvar lusty-file-face font-lock-string-face)

(defvar lusty-buffer-name " *Lusty-Matches*")
(defvar lusty-prompt ">> ")
(defvar lusty-column-separator "    ")
(defvar lusty-no-matches-string
  (propertize "-- NO MATCHES --" 'face 'font-lock-warning-face))
(defvar lusty-truncated-string
  (propertize "-- TRUNCATED --" 'face 'font-lock-comment-face))

(defvar lusty-mode-map nil
  "Minibuffer keymap for `lusty-file-explorer' and `lusty-buffer-explorer'.")

;;;###autoload
(defun lusty-file-explorer ()
  "Launch the file/directory mode of LustyExplorer."
  (interactive)
  (lusty--define-mode-map)
  (let* ((lusty--active-mode :file-explorer)
         (lusty--ignored-extensions-regex
           (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
         (minibuffer-local-filename-completion-map lusty-mode-map)
         (file
          ;; read-file-name is silly in that if the result is equal to the
          ;; dir argument, it gets converted to the default-filename
          ;; argument.  Set it explicitly to "" so if lusty-launch-dired is
          ;; called in the directory we start at, the result is that directory
          ;; instead of the name of the current buffer.
          (lusty--run 'read-file-name default-directory "")))
    (when file
      (switch-to-buffer
       (find-file-noselect
        (expand-file-name file))))))

;;;###autoload
(defun lusty-buffer-explorer ()
  "Launch the buffer mode of LustyExplorer."
  (interactive)
  (lusty--define-mode-map)
  (let* ((lusty--active-mode :buffer-explorer)
         (minibuffer-local-completion-map lusty-mode-map)
         (buffer (lusty--run 'read-buffer)))
    (when buffer
      (switch-to-buffer buffer))))

;;;###autoload
(defun lusty-highlight-next ()
  "Highlight the next match in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty-propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (incf y)
      (unless (lusty--matrix-coord-valid-p x y)
        (incf x)
        (setq y 0)
        (unless (lusty--matrix-coord-valid-p x y)
          (setq x 0)))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-highlight-previous ()
  "Highlight the previous match in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty-propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (decf y)
      (unless (lusty--matrix-coord-valid-p x y)
        (let ((n-cols (length lusty--matches-matrix))
              (n-rows (length (aref lusty--matches-matrix 0))))
          (decf x)
          (setq y (1- n-rows))
          (unless (lusty--matrix-coord-valid-p x y)
            (setq x (1- n-cols))
            (while (not (lusty--matrix-coord-valid-p x y))
              (decf y)))))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-highlight-next-column ()
  "Highlight the next column in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty-propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (incf x)
      (unless (lusty--matrix-coord-valid-p x y)
        (setq x 0)
        (incf y)
        (unless (lusty--matrix-coord-valid-p x y)
          (setq y 0)))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-highlight-previous-column ()
  "Highlight the previous column in *Lusty-Matches*."
  (interactive)
  (when (and lusty--active-mode
             (not (lusty--matrix-empty-p)))
    (destructuring-bind (x . y) lusty--highlighted-coords

      ;; Unhighlight previous highlight.
      (let ((prev-highlight
             (aref (aref lusty--matches-matrix x) y)))
        (lusty-propertize-path prev-highlight))

      ;; Determine the coords of the next highlight.
      (let ((n-cols (length lusty--matches-matrix))
            (n-rows (length (aref lusty--matches-matrix 0))))
        (if (and (zerop x)
                 (zerop y))
            (progn
              (setq x (1- n-cols)
                    y (1- n-rows))
              (while (not (lusty--matrix-coord-valid-p x y))
                (decf y)))
          (decf x)
          (unless (lusty--matrix-coord-valid-p x y)
            (setq x (1- n-cols))
            (decf y)
            (unless (lusty--matrix-coord-valid-p x y)
              ;            (setq y (1- n-rows))
              (while (not (lusty--matrix-coord-valid-p x y))
                (decf x))))))

      ;; Refresh with new highlight.
      (setq lusty--highlighted-coords (cons x y))
      (lusty-refresh-matches-buffer :use-previous-matrix))))

;;;###autoload
(defun lusty-select-match ()
  "Select the highlighted match in *Lusty-Matches*."
  (interactive)
  (destructuring-bind (x . y) lusty--highlighted-coords
    (when (and lusty--active-mode
               (< x (length lusty--matches-matrix))
               (< y (length (aref lusty--matches-matrix x))))
      (let ((selected-match
             (aref (aref lusty--matches-matrix x) y)))
        (ecase lusty--active-mode
          (:file-explorer (lusty--file-explorer-select selected-match))
          (:buffer-explorer (lusty--buffer-explorer-select selected-match)))))))

;;;###autoload
(defun lusty-select-current-name ()
  "Open the given file/buffer or create a new buffer with the current name."
  (interactive)
  (when lusty--active-mode
    (exit-minibuffer)))

;;;###autoload
(defun lusty-launch-dired ()
  "Launch dired at the current directory."
  (interactive)
  (when (eq lusty--active-mode :file-explorer)
    (let* ((path (minibuffer-contents-no-properties))
           (dir (lusty-normalize-dir (file-name-directory path))))
      (lusty-set-minibuffer-text dir)
      (exit-minibuffer))))

;; TODO:
;; - highlight opened buffers in filesystem explorer
;; - FIX: deal with permission-denied
;; - if NO ENTRIES, RET opens new buffer with current name (if nonempty)
;; - C-e/C-a -> last/first column?
;; - config var: C-x d opens highlighted dir instead of current dir
;; - (run-with-idle-timer 0.1 ...)

(defvar lusty--active-mode nil)
(defvar lusty--wrapping-ido-p nil)
(defvar lusty--initial-window-config nil)
(defvar lusty--previous-minibuffer-contents nil)
(defvar lusty--ignored-extensions-regex
  ;; Recalculated at execution time.
  (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))

(defvar lusty--highlighted-coords (cons 0 0))  ; (x . y)

;; Set by lusty--compute-layout-matrix
(defvar lusty--matches-matrix (make-vector 0 nil))
(defvar lusty--matrix-column-widths '())
(defvar lusty--matrix-truncated-p nil)

(defconst lusty--greatest-factors
  (let ((vec (make-vector 1000 nil)))
    (dotimes (n 1000)
      (let ((factor
             (loop for i from 2 upto (ash n -1)
                   when (zerop (mod n i))
                   return (/ n i))))
      (aset vec n factor)))
    vec))

(when lusty--wrapping-ido-p
  (require 'ido))
(defvar ido-text) ; silence compiler warning

(defsubst lusty--matrix-empty-p ()
  (zerop (length lusty--matches-matrix)))
(defsubst lusty--matrix-coord-valid-p (x y)
  (not (or (minusp x)
           (minusp y)
           (>= x (length lusty--matches-matrix))
           (>= y (length (aref lusty--matches-matrix 0)))
           (null (aref (aref lusty--matches-matrix x) y)))))

(defun lusty-sort-by-fuzzy-score (strings abbrev)
  ;; TODO: case-sensitive when abbrev contains capital letter
  (let* ((strings+scores
          (loop for str in strings
                for score = (LM-score str abbrev)
                unless (zerop score)
                collect (cons str score)))
         (sorted
          (sort* strings+scores '> :key 'cdr)))
    (mapcar 'car sorted)))

(defun lusty-normalize-dir (dir)
  "Clean up the given directory path."
  (if (and dir (plusp (length dir)))
      (setq dir (abbreviate-file-name
                 (expand-file-name
                  (substitute-in-file-name dir))))
    (setq dir "."))
  (and (file-directory-p dir)
       dir))

(defun lusty-complete-env-variable (path)
  "Look for an environment variable in PATH and try to complete it as
much as possible."
  (when (string-match "\$\\([[:word:]_]+\\)" path)
    (let* ((partial-var (match-string 1 path))
           (vars (mapcar (lambda (x)
                           (string-match "^[^=]+" x)
                           (match-string 0 x))
                         (remove-if-not
                          (lambda (x)
                            (string-match (concat "^" partial-var) x))
                          process-environment)))
           (longest-completion (try-completion partial-var vars)))
      (cond ((eq t longest-completion) nil)
            ((null longest-completion) nil)
            ((> (length longest-completion) (length partial-var))
             (replace-regexp-in-string (concat "\$" partial-var)
                                       (concat "\$" longest-completion)
                                       path t t))))))

(defun lusty-filter-buffers (buffers)
  "Return BUFFERS converted to strings with hidden buffers removed."
  (macrolet ((ephemeral-p (name)
               `(eq (string-to-char ,name) ?\ )))
    (loop for buffer in buffers
          for name = (buffer-name buffer)
          unless (ephemeral-p name)
          collect name)))

;; Written kind-of silly for performance.
(defun lusty-filter-files (file-portion files)
  "Return FILES with './' removed and hidden files if FILE-PORTION
does not begin with '.'."
  (macrolet ((leading-dot-p (str)
               `(eq (string-to-char ,str) ?.))
             (pwd-p (str)
               `(string= ,str "./"))
             (ignored-p (name)
               `(string-match lusty--ignored-extensions-regex ,name)))
    (let ((filtered-files '()))
      (if (leading-dot-p file-portion)
          (dolist (file files)
            (unless (or (pwd-p file)
                        (ignored-p file))
              (push file filtered-files)))
        (dolist (file files)
          (unless (or (leading-dot-p file)
                      (ignored-p file))
            (push file filtered-files))))
      (nreverse filtered-files))))

(defun lusty-set-minibuffer-text (&rest args)
  "Sets ARGS into the minibuffer after the prompt."
  (assert (minibufferp))
  (delete-region (minibuffer-prompt-end) (point-max))
  (apply 'insert args))

(defun lusty--file-explorer-select (match)
  (let* ((path (minibuffer-contents-no-properties))
         (var-completed-path (lusty-complete-env-variable path)))
    (if var-completed-path
        ;; We've completed a variable name (at least partially) -- set it and
        ;; leave, since it's confusing to do two kinds of completion at once.
        (lusty-set-minibuffer-text var-completed-path)
      (let* ((dir (file-name-directory path))
             (file-portion (file-name-nondirectory path))
             (normalized-dir (lusty-normalize-dir dir)))
        ;; Clean up the path when selecting, in case we recurse.
        (lusty-set-minibuffer-text normalized-dir match)
        (if (file-directory-p (concat normalized-dir match))
            (progn
              (setq lusty--highlighted-coords (cons 0 0))
              (lusty-refresh-matches-buffer))
          (minibuffer-complete-and-exit))))))

(defun lusty--buffer-explorer-select (match)
  (lusty-set-minibuffer-text match)
  (minibuffer-complete-and-exit))

;; This may seem overkill, but it's the only way I've found to update the
;; matches list for every edit to the minibuffer.  Wrapping the keymap can't
;; account for user bindings or commands and would also fail for viper.
(defun lusty--post-command-function ()
  (assert lusty--active-mode)
  (when (and (minibufferp)
             (or (null lusty--previous-minibuffer-contents)
                 (not (string= lusty--previous-minibuffer-contents
                               (minibuffer-contents-no-properties)))))

    (when (null lusty--initial-window-config)
      ;; (Only run when the explorer function is initially executed.)
      (lusty--setup-matches-window))

    (setq lusty--previous-minibuffer-contents (minibuffer-contents-no-properties)
          lusty--highlighted-coords (cons 0 0))
    (lusty-refresh-matches-buffer)))

;; Cribbed with modification from tail-select-lowest-window.
(defun lusty-lowest-window ()
  "Return the lowest window on the frame."
  (let* ((current-window (if (minibufferp)
                             (next-window (selected-window) :skip-mini)
                           (selected-window)))
         (lowest-window current-window)
         (bottom-edge (fourth (window-pixel-edges current-window)))
         (last-window (previous-window current-window :skip-mini))
         (window-search-p t))
    (while window-search-p
      (let* ((this-window (next-window current-window :skip-mini))
             (next-bottom-edge (fourth (window-pixel-edges this-window))))
        (when (< bottom-edge next-bottom-edge)
          (setq bottom-edge next-bottom-edge)
          (setq lowest-window this-window))
        (setq current-window this-window)
        (when (eq last-window this-window)
          (setq window-search-p nil))))
    lowest-window))

(defun lusty-max-window-height ()
  "Return the expected maximum allowable height of a window on this frame"
  ;; FIXME: are there cases where this is incorrect?
  (let* ((lusty-window
          (get-buffer-window
           (get-buffer-create lusty-buffer-name)))
         (other-window
          ;; In case the *LustyMatches* window was closed
          (or lusty-window
              (if (minibufferp)
                  (next-window (selected-window) :skip-mini)
                (selected-window))))
         (test-window
          (or lusty-window other-window)))
    (assert test-window)
    (- (frame-height)
       ;; Account for modeline and/or header...
       (- (window-height test-window)
          (window-body-height test-window))
       ;; And minibuffer height.
       (window-height (minibuffer-window)))))

(defun lusty-max-window-width ()
  (frame-width))

(defun lusty--setup-matches-window ()
  (let ((lowest-window (lusty-lowest-window))
        (lusty-buffer (get-buffer-create lusty-buffer-name)))
    (save-selected-window
      (select-window lowest-window)
      (let ((new-lowest
             ;; Create the window for lusty-buffer
             (split-window-vertically)))
        (select-window new-lowest)
        ;; Try to get a window covering the full frame.  Sometimes
        ;; this takes more than one try, but we don't want to do it
        ;; infinitely in case of weird setups.
        (loop repeat 5
              while (< (window-width) (frame-width))
              do
              (condition-case nil
                  (enlarge-window-horizontally (- (frame-width)
                                                  (window-width)))
                (error
                 (return))))
        (set-window-buffer new-lowest lusty-buffer))))
  ;;
  ;; Window configuration may be restored intermittently.
  (setq lusty--initial-window-config (current-window-configuration)))

(defun lusty-refresh-matches-buffer (&optional use-previous-matrix-p)
  "Refresh *Lusty-Matches*."
  (assert (minibufferp))
  (let* ((minibuffer-text (if lusty--wrapping-ido-p
                              ido-text
                            (minibuffer-contents-no-properties))))

    (unless use-previous-matrix-p
      ;; Refresh the matches and layout matrix
      (let ((matches
             (ecase lusty--active-mode
               (:file-explorer
                (lusty-file-explorer-matches minibuffer-text))
               (:buffer-explorer
                (lusty-buffer-explorer-matches minibuffer-text)))))
        (lusty--compute-layout-matrix matches)))

    ;; Update the matches window.
    (let ((lusty-buffer (get-buffer-create lusty-buffer-name)))
      (with-current-buffer lusty-buffer
        (setq buffer-read-only t)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (lusty--display-matches)
          (goto-char (point-min))))

      ;; If only our matches window is open,
      (when (one-window-p t)
        ;; Restore original window configuration before fitting the
        ;; window so the minibuffer won't grow and look silly.
        (set-window-configuration lusty--initial-window-config))
      (fit-window-to-buffer (display-buffer lusty-buffer))
      (set-buffer-modified-p nil))))

(defun lusty-buffer-explorer-matches (text)
  (let* ((buffers (lusty-filter-buffers (buffer-list))))
    (if (string= text "")
        buffers
      (lusty-sort-by-fuzzy-score
       buffers
       text))))

;; FIXME: return an array instead of a list?
(defun lusty-file-explorer-matches (path)
  (let* ((dir (lusty-normalize-dir (file-name-directory path)))
         (file-portion (file-name-nondirectory path))
         (files
          (and dir
               ; NOTE: directory-files is quicker but
               ;       doesn't append slash for directories.
               ;(directory-files dir nil nil t)
               (file-name-all-completions "" dir)))
         (filtered (lusty-filter-files file-portion files)))
    (if (or (string= file-portion "")
            (string= file-portion "."))
        (sort filtered 'string<)
      (lusty-sort-by-fuzzy-score filtered file-portion))))

(defsubst lusty-propertize-path (path)
  "Propertize the given PATH like so: <dir></> or <file>.
Uses `lusty-directory-face', `lusty-slash-face', `lusty-file-face'"
  (let ((last (1- (length path))))
    ;; Note: shouldn't get an empty path, so for performance
    ;; I'm not going to check for that case.
    (if (eq (aref path last) ?/) ; <-- FIXME nonportable?
        (progn
          ;; Directory
          (put-text-property 0 last 'face lusty-directory-face path)
          (put-text-property last (1+ last) 'face lusty-slash-face path))
      (put-text-property 0 (1+ last) 'face lusty-file-face path)))
  path)

(defun lusty--compute-layout-matrix (items)
  (let* ((max-visible-rows (1- (lusty-max-window-height)))
         (max-width (lusty-max-window-width))
         (upper-bound most-positive-fixnum)
         (n-items (length items))
         (lengths-v (make-vector n-items 0))
         (separator-length (length lusty-column-separator)))

    (let ((length-of-longest-name 0)) ; used to determine upper-bound

      ;; Initialize lengths-v
      (loop for i from 0
            for item in items
            for len = (length item)
            do
            (aset lengths-v i len)
            (setq length-of-longest-name
                  (max length-of-longest-name len)))

      ;; Calculate upper-bound
      (let ((width (+ length-of-longest-name
                      separator-length))
            (columns 1)
            (sorted-shortest (sort (append lengths-v nil) '<)))
        (dolist (item-len sorted-shortest)
          (incf width item-len)
          (when (> width max-width)
            (return))
          (incf columns)
          (incf width separator-length))
        (setq upper-bound (* columns max-visible-rows))))

    ;; Determine optimal row count.
    (multiple-value-bind (optimal-n-rows truncated-p)
        (cond ((endp items)
               (values 0 nil))
              ((< upper-bound n-items)
               (values max-visible-rows t))
              ((<= (reduce (lambda (a b) (+ a separator-length b))
                           lengths-v)
                   max-width)
               ;; All fits in a single row.
               (values 1 nil))
              (t
               (lusty--compute-optimal-row-count lengths-v
                                                 separator-length)))
      (let ((n-columns 0)
            (column-widths '()))

        ;; Calculate n-columns and column-widths
        (loop with total-width = 0
              for start = 0 then end
              for end = optimal-n-rows then
                        (min (length lengths-v)
                             (+ end optimal-n-rows))
              while (< start end)
              for col-width = (reduce 'max lengths-v
                                      :start start
                                      :end end)
              do
              (incf total-width col-width)
              (when (> total-width max-width)
                (return))
              (incf n-columns)
              (push col-width column-widths)
              (incf total-width separator-length))
        (setq column-widths (nreverse column-widths))

        (let ((matrix
               ;; Create an empty matrix.
               (let ((col-vec (make-vector n-columns nil)))
                 (dotimes (i n-columns)
                   (aset col-vec i
                         (make-vector optimal-n-rows nil)))
                 col-vec)))

          ;; Fill the matrix with propertized matches.
          (unless (zerop n-columns)
            (let ((x 0)
                  (y 0)
                  (col-vec (aref matrix 0)))
              (dolist (item items)
                (aset col-vec y (lusty-propertize-path item))
                (incf y)
                (when (>= y optimal-n-rows)
                  (incf x)
                  (if (>= x n-columns)
                      (return)
                    (setq col-vec (aref matrix x)))
                  (setq y 0)))))

          (setq lusty--matches-matrix matrix
                lusty--matrix-column-widths column-widths
                lusty--matrix-truncated-p truncated-p))))))

;; Returns number of rows and whether this truncates the matches.
(defun* lusty--compute-optimal-row-count (lengths-v separator-length)
  (let* ((n-items (length lengths-v))
         (max-visible-rows (1- (lusty-max-window-height)))
         (available-width (lusty-max-window-width))
         (lengths-h (make-hash-table :test 'equal
                                     ; not scientific
                                     :size n-items)))

    ;; FIXME: do binary search instead of linear
    (do ((n-rows 2 (1+ n-rows)))
        ((>= n-rows max-visible-rows)
         (values max-visible-rows t))
      (let ((col-start-index 0)
            (col-end-index (1- n-rows))
            (total-width 0)
            (split-factor (aref lusty--greatest-factors n-rows)))

        ;; Calculate required total-width for this number of rows.
        (while (< col-end-index n-items)
          (let ((column-width
                 (lusty--compute-column-width
                  col-start-index col-end-index split-factor
                  lengths-v lengths-h)))

            (incf total-width column-width)
            (incf total-width separator-length))

          (incf col-start-index n-rows) ; setq col-end-index
          (incf col-end-index n-rows)

          (when (and (>= col-end-index n-items)
                     (< col-start-index n-items))
            ;; Remainder; last iteration will not be a full column.
            (setq col-end-index (1- n-items)
                  split-factor nil)))

        ;; The final column doesn't need a separator.
        (decf total-width separator-length)

        (when (<= total-width available-width)
          (return-from lusty--compute-optimal-row-count
            (values n-rows nil)))))))

(defsubst lusty--compute-column-width (start-index end-index split-factor
                                       lengths-v lengths-h)
  (let ((width 0)
        (iter start-index))
    (cond ((= start-index end-index)
           ;; Single-element remainder
           (setq width (aref lengths-v iter)))
          ((null split-factor)
           ;; Prime number, or a remainder
           (while (<= iter end-index)
             (setq width (max width (aref lengths-v iter)))
             (incf iter)))
          (t
           (while (<= iter end-index)
             (setq width
                   (max width
                        (gethash (cons iter (+ iter (1- split-factor))) lengths-h)))
             (incf iter split-factor))))
    (puthash (cons start-index end-index) width lengths-h)
    width))

(defun* lusty--display-matches ()

  (when (lusty--matrix-empty-p)
    (lusty--print-no-matches)
    (return-from lusty--display-matches))

  (let* ((n-columns (length lusty--matches-matrix))
         (n-rows (length (aref lusty--matches-matrix 0))))

    ;; Highlight the selected match.
    (destructuring-bind (h-x . h-y) lusty--highlighted-coords
      (setf (aref (aref lusty--matches-matrix h-x) h-y)
            (propertize (aref (aref lusty--matches-matrix h-x) h-y)
                        'face lusty-match-face)))

    ;; Print the match matrix.
    (dotimes (y n-rows)
      (loop for column-width in lusty--matrix-column-widths
            for x from 0 upto n-columns
            do
            (let ((match (aref (aref lusty--matches-matrix x) y)))
              (when match
                (insert match)
                (when (< x (1- n-columns))
                  (let* ((spacer
                          (make-string (- column-width (length match))
                                       ?\ )))
                    (insert spacer lusty-column-separator))))))
      (insert "\n")))

  (when lusty--matrix-truncated-p
    (lusty--print-truncated)))

(defun lusty--print-no-matches ()
  (insert lusty-no-matches-string)
  (let ((fill-column (window-width)))
    (center-line)))

(defun lusty--print-truncated ()
  (insert lusty-truncated-string)
  (let ((fill-column (window-width)))
    (center-line)))


(defun lusty--define-mode-map ()
  ;; Re-generated every run so that it can inherit new functions.
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    ;; TODO: perhaps RET should be:
    ;; - if buffer explorer, same as \t
    ;; - if file explorer, opens current name (or recurses if existing dir)
    (define-key map (kbd "RET") 'lusty-select-match)
    (define-key map "\t" 'lusty-select-match)
    (define-key map "\C-n" 'lusty-highlight-next)
    (define-key map "\C-p" 'lusty-highlight-previous)
    (define-key map "\C-f" 'lusty-highlight-next-column)
    (define-key map "\C-b" 'lusty-highlight-previous-column)
    (define-key map "\C-xd" 'lusty-launch-dired)
    (define-key map "\C-xe" 'lusty-select-current-name)
    (setq lusty-mode-map map))
  (run-hooks 'lusty-setup-hook))


(defun lusty--run (read-fn &rest args)
  (let ((lusty--highlighted-coords (cons 0 0))
        (lusty--matches-matrix (make-vector 0 nil))
        (lusty--matrix-column-widths '())
        (lusty--matrix-truncated-p nil))
    (add-hook 'post-command-hook 'lusty--post-command-function t)
    (unwind-protect
        (save-window-excursion
          (apply read-fn lusty-prompt args))
      (remove-hook 'post-command-hook 'lusty--post-command-function)
      (setq lusty--previous-minibuffer-contents nil
            lusty--initial-window-config nil))))


;;
;; Start LiquidMetal
;;
;; Port of Ryan McGeary's LiquidMetal fuzzy matching algorithm found at:
;;   http://github.com/rmm5t/liquidmetal/tree/master.
;;

(defconst LM--score-no-match 0.0)
(defconst LM--score-match 1.0)
(defconst LM--score-trailing 0.8)
(defconst LM--score-trailing-but-started 0.90)
(defconst LM--score-buffer 0.85)

(defsubst* LM-score (str abbrev)
  (let ((str-len (length str))
        (abbrev-len (length abbrev)))
    (cond ;((string= abbrev "")  ; Disabled; can't happen in practice
          ; LM--score-trailing)
          ((> abbrev-len str-len)
           LM--score-no-match)
          (t
           ;; Content of LM--build-score-array...
           ;; Inline for interpreted performance.
           (let* ((scores (make-vector str-len LM--score-no-match))
                  (str-lower (downcase str))
                  (abbrev-lower (downcase abbrev))
                  (last-index 0)
                  (started-p nil))
             (dotimes (i abbrev-len)
               (let ((pos (position (aref abbrev-lower i) str-lower
                                    :start last-index
                                    :end str-len)))
                 (when (null pos)
                   (return-from LM-score LM--score-no-match))
                 (when (zerop pos)
                   (setq started-p t))
                 (cond ((and (plusp pos)
                             (memq (aref str (1- pos))
                                   '(?. ?_ ?- ?\ )))
                        ;; New word.
                        (aset scores (1- pos) LM--score-match)
                        (fill scores LM--score-buffer
                              :start last-index
                              :end (1- pos)))
                       ((and (>= (aref str pos) ?A)
                             (<= (aref str pos) ?Z))
                        ;; Upper case.
                        (fill scores LM--score-buffer
                              :start last-index
                              :end pos))
                       (t
                        (fill scores LM--score-no-match
                              :start last-index
                              :end pos)))
                 (aset scores pos LM--score-match)
                 (setq last-index (1+ pos))))

             (let ((trailing-score
                    (if started-p
                        LM--score-trailing-but-started
                      LM--score-trailing)))
               (fill scores trailing-score :start last-index))

             (/ (reduce '+ scores)
                str-len ))))))

;;
;; End LiquidMetal
;;


;;
;; XEmacs compatibility functions
;;

; (unless (fboundp 'minibufferp)
;   (defun minibufferp ()
;     (eq (window-buffer (minibuffer-window))
;         (current-buffer))))
; 
; (unless (fboundp 'minibuffer-contents-no-properties)
;   (defun minibuffer-contents-no-properties ()
;     (with-current-buffer (window-buffer (minibuffer-window))
;       (let ((start (1+ (length lusty-prompt)))
;             (end (point-max)))
;         (if (>= end start)
;             (buffer-substring-no-properties start end)
;           "")))))
; 
; (unless (fboundp 'minibuffer-prompt-end)
;   (defun minibuffer-prompt-end ()
;     (1+ (length lusty-prompt))))
; 
; (unless (fboundp 'line-number-at-pos)
;   (defun line-number-at-pos (&optional pos)
;     (line-number pos)))
; 
; ;; Cribbed from cal-fit-window-to-buffer
; (unless (fboundp 'fit-window-to-buffer)
;   (defun fit-window-to-buffer (owin max-height)
;     (interactive)
;     (if owin
; 	(delete-other-windows))
;     (when (> (length (window-list nil 'nomini)) 1)
;       (let* ((window (selected-window))
; 	     (buf (window-buffer window))
; 	     (height (window-displayed-height (selected-window)))
; 	     (new-height
;               (min (with-current-buffer buf
;                      (count-lines (point-min) (point-max)))
;                    max-height))
; 	     (diff (- new-height height)))
; 	(unless (zerop diff)
; 	  (enlarge-window diff))
; 	(let ((end (with-current-buffer buf (point-max))))
; 	  (while (and (> (length (window-list nil 'nomini)) 1)
; 		      (not (pos-visible-in-window-p end)))
; 	    (enlarge-window 1)))))))


(provide 'lusty-explorer)

;;; lusty-explorer.el ends here.
