(when (file-exists-p "~/ESS")
  (add-to-list 'load-path "~/ESS/lisp/")
  ;(load "ess-site")
  ;(require 'ess-site)
  (require 'ess-r-mode)
  ;; Don't convert '_' to '<-'.  Annoying.
  (ess-toggle-underscore nil)
  ;; This interferes with my `evil` configuration.
  (define-key inferior-ess-mode-map "," nil))

