(use-package flyspell
  :config
  (progn
    (bind-keys :map flyspell-mode-map
               ("C-." . nil)
               ("C-," . nil))

    (dolist (hook '(text-mode-hook
                    org-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

    (dolist (mode '(emacs-lisp-mode-hook
                    inferior-lisp-mode-hook
                    clojure-mode-hook))
      (add-hook mode
                '(lambda ()
                   (flyspell-prog-mode))))

    (use-package helm-flyspell
      :config
      (bind-keys :map flyspell-mode-map
                 ("C-;" . helm-flyspell-correct)))))

(use-package ispell
  :config
  (bind-keys :map global-map
             ("<f12>" . ispell-word)))
;; (global-set-key (kbd "<f8>") 'ispell-word)
;; (defun flyspell-check-next-highlighted-word ()
;;   "Custom function to spell check next highlighted word"
;;   (interactive)
;;   (flyspell-goto-next-error)
;;   (ispell-word))
;; (global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)
