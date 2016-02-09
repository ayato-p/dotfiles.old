(use-package geiser-mode
  :config
  (progn
    (bind-keys :map geiser-mode-map
               ("C-c M-j" . run-geiser)
               ("C-c C-l" . geiser-load-current-buffer)
               ("C-." . nil))

    (setq geiser-active-implementations '(racket))
    (add-hook 'geiser-mode-hook 'my/geiser-mode-hook)
    (add-hook 'geiser-repl-mode-hook 'my/geiser-mode-hook)

    (defun my/geiser-mode-hook ()
      (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
      (my/lisp-mode-defaults))))
