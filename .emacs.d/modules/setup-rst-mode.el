(use-package rst
  :config
  (progn
    (use-package auto-complete-rst
      :config (auto-complete-rst-init))

    (setq auto-mode-alist
          (append '(("\\.rst$" . rst-mode)
                    ("\\.rest$" . rst-mode)) auto-mode-alist))
    (setq frame-background-mode 'dark)
    (add-hook 'rst-mode-hook '(lambda()
                                (setq indent-tabs-mode nil)
                                ))))
