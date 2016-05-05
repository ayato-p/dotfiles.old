(use-package scss-mode
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-css))))))
