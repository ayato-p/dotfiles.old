;;; setup-lisp.el --- lisp-mode config.

;;; Commentary:

;;; Code:

;; paredit
(use-package paredit
  :defer t
  :config (progn
            (bind-keys :map paredit-mode-map
                       ("C-h" . paredit-backward-delete))

            ;; Enable `paredit-mode' in the minibuffer, during `eval-expression'.
            (defun conditionally-enable-paredit-mode ()
              (if (eq this-command 'eval-expression)
                  (paredit-mode 1)))
            (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)))

;; el-doc
(use-package eldoc
  :defer t
  :config (setq eldoc-idle-delay 0.2
                eldoc-minor-mode-string ""))

(defun my/lisp-mode-defaults ()
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (eldoc-mode 1))

(defun my/lisp-mode-hook ()
  (my/lisp-mode-defaults))

(defun my/emacs-lisp-mode-hook ()
  (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
  (my/lisp-mode-defaults))

(add-hook 'emacs-lisp-mode-hook 'my/emacs-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my/lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my/lisp-mode-hook)
(add-hook 'ielm-mode-hook 'my/lisp-mode-hook)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\.stumpwmrc" . common-lisp-mode))

;; C-x F, C-x K, C-x V
(find-function-setup-keys)

;;; setup-lisp.el ends here
