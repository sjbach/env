;; This file isn't loaded, it's just for posterity / searchability.
(assert false)

; Slime autodoc used to expand the minibuffer height, which was annoying.
(setq resize-mini-windows nil)

;; Turn on font-lock mode for syntax highlighting
;; (These are now defaults)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Transient mark mode is now a default (and I think this was accidentally
;; disabling it, not enabling it).
(transient-mark-mode t)
