;;; -*- lexical-binding: t; -*-
;;
;; Key translation, key maps, key definitions
;;
;; Note: other files also contain key maps and key definitions.
;;

(require 'hydra)

;; Hide-show
(defhydra steve-hydra-hs (:pre (hs-minor-mode 1)
                          :foreign-keys run)
   ("h" hs-hide-all "hide all" :column "Hide")
   ("d" hs-hide-block "hide block")
   ("l" hs-hide-level "hide level")
   ("s" hs-show-all "show all" :column "Show")
   ("a" hs-show-block "show block")
   ("t" hs-toggle-hiding "toggle" :column "Toggle")
   ("q" nil))

;; Elisp-ref
(defhydra steve-hydra-elisp-refs ()
  "\nelisp-refs: <%s(symbol-at-point)>\n"
   ("a" (elisp-refs-symbol (symbol-at-point)) "all syms")
   ("f" (elisp-refs-function (symbol-at-point)) "funcs")
   ("m" (elisp-refs-macro (symbol-at-point)) "macros")
   ("v" (elisp-refs-variable (symbol-at-point)) "vars")
   ("s" (elisp-refs-special (symbol-at-point)) "specials")
   ("q" nil))


