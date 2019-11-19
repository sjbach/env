;;; -*- lexical-binding: t; -*-
;; (Ancient.)

(when (file-exists-p "~/clojure")
  ;; TODO: cleanup
  ;(add-to-list 'load-path "~/clojure/swank-clojure")
  ;(setq swank-clojure-jar-path "~/clojure/clojure-git/clojure.jar")
  ;(require 'swank-clojure-autoload)

;  (eval-after-load "slime"
;    '(add-to-list 'slime-lisp-implementations '(sbcl ("sbcl"))))

  (add-to-list 'load-path "~/clojure/clojure-mode")
  (autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
  (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode)))

