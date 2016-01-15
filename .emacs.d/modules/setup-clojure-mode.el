;;; setup-clojure-mode.el --- clojure-mode-settings

;;; Commentary:

;;; Code:

;; Clojure
(use-package clojure-mode
  :config
  (progn
    (use-package smart-newline-mode)
    (use-package clojure-mode-extra-font-locking)
    (use-package midje-mode)
    (use-package clj-refactor
      :diminish clj-refactor-mode
      :config (progn
                (cljr-add-keybindings-with-prefix "C-c j")
                (setq cljr-eagerly-build-asts-on-startup nil)
                (setq cljr-populate-artifact-cache-on-startup nil)
                (setq cljr-magic-requires t)))
    (use-package cider
      :config (progn
                (setq nrepl-log-messages t)
                (setq nrepl-hide-special-buffers t)
                (setq cider-repl-history-file (locate-user-emacs-file ".nrepl-history"))
                (setq cider-eval-result-prefix "➫ ")

                (custom-set-faces
                 '(cider-result-overlay-face
                   ((((class color) (background dark)) (:foreground "#DCDCCC"))))) ; same as zenburn-fg

                (use-package cider-eldoc)

                (use-package company
                  :init
                  (add-hook 'cider-repl-mode-hook #'company-mode)
                  (add-hook 'cider-mode-hook #'company-mode))
                ;; (use-package ac-cider
                ;;   :init
                ;;   (progn
                ;;     (eval-after-load "auto-complete"
                ;;       '(progn (add-to-list 'ac-modes 'cider-mode)
                ;;               (add-to-list 'ac-modes 'cider-repl-mode)))))

                (bind-keys :map cider-mode-map
                           ("C-x *" . my/zou-go))

                (defun my/cider-mode-hook ()
                  (paredit-mode 1)
                  (rainbow-delimiters-mode 1)
                  (cider-turn-on-eldoc-mode)
                  (ac-flyspell-workaround) ; ?

                  (ac-cider-setup))

                (add-hook 'cider-mode-hook 'my/cider-mode-hook)
                (add-hook 'cider-repl-mode-hook 'my/cider-mode-hook)

                (defun my/cider-namespace-refresh ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'clojure.tools.namespace.repl)(clojure.tools.namespace.repl/refresh)"))

                (defun my/cider-reload-project ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'alembic.still)(alembic.still/load-project)"))

                (defun my/cider-midje-run-autotest ()
                  (interactive)
                  (cider-interactive-eval
                   "(require 'midje.repl)(midje.repl/autotest)"))

                (defun my/zou-go ()
                  (interactive)
                  (if current-prefix-arg
                      (progn
                        (save-some-buffers)
                        (cider-interactive-eval
                         "(zou.framework.repl/reset)"))
                    (cider-interactive-eval
                     "(zou.framework.repl/go)")))))

    (bind-keys :map clojure-mode-map
               ("C-c j a l" . clojure-align))

    (defun my/clojure-mode-hook ()
      (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
      (clj-refactor-mode 1)
      (paredit-mode 1)
      (rainbow-delimiters-mode 1))

    (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)

    (define-clojure-indent
      (defroutes 'defun)
      (GET 2)
      (POST 2)
      (PUT 2)
      (DELETE 2)
      (HEAD 2)
      (ANY 2)
      (context 2)
      (let-test-data 1)
      (for-all 1))))

;;; setup-clojure-mode.el ends here
