(add-hook 'html-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (set-fill-column 100)))
(add-hook 'sgml-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (set-fill-column 100)))
(add-hook 'python-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (set-fill-column 80)))
(add-hook 'c++-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (set-fill-column 80)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq fill-column 79)))

