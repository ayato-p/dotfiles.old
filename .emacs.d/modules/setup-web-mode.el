(use-package web-mode
  :mode (("\\.html\\.twig\\'" . web-mode)
         ("\\.html\\'"        . web-mode))
  :init (progn
          (setq web-mode-enable-auto-pairing nil)))
