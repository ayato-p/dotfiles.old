;;; setup-clojure-mode.el --- clojure-mode-settings

;;; Commentary:

;;; Code:

;; Clojure
(use-package clojure-mode
  :config
  (progn
    ;; (use-package cider-eval-sexp-fu
    ;;     :config
    ;;     (progn
    ;;       (setq eval-sexp-fu-flash-duration 1)
    ;;       (custom-set-faces
    ;;        '(eval-sexp-fu-flash
    ;;          ((((class color)) (:background "#F0DFAF" :foreground "#3F3F3F")))))))
    (use-package clojure-mode-extra-font-locking)
    (use-package midje-mode)
    (use-package clj-refactor
      :diminish clj-refactor-mode
      :config (progn
                (cljr-add-keybindings-with-prefix "C-c j")
                (setq cljr-eagerly-build-asts-on-startup nil)
                (setq cljr-populate-artifact-cache-on-startup nil)
                (setq cljr-magic-requires t)
                (setq cljr-favor-prefix-notation nil)
                (setq cljr-warn-on-eval nil)
                (setq cljr-auto-clean-ns nil)))
    (use-package cider
      :config (progn
                (setq nrepl-log-messages t)
                (setq nrepl-hide-special-buffers t)
                (setq cider-repl-display-help-banner nil)
                (setq cider-repl-history-file (locate-user-emacs-file ".nrepl-history"))
                (setq cider-eval-result-prefix "➫ ")
                (setq cider-connection-message-fn #'cider-random-tip)

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
                  (eldoc-mode +1)
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

    (defvar my/clojure-prettify-alist '())
    (add-to-list 'my/clojure-prettify-alist '(">=" . ?≧))
    (add-to-list 'my/clojure-prettify-alist '(">=" . ?≦))

    (setq clojure--prettify-symbols-alist
          (append my/clojure-prettify-alist
                  clojure--prettify-symbols-alist))

    (defun my/clojure-mode-custom-indent ()
      (put-clojure-indent 'fnk 'defun)
      (put-clojure-indent 'defnk 'defun)
      (put-clojure-indent 'for-map 1)
      (put-clojure-indent 'instance 2)
      (put-clojure-indent 'inline 1)
      (put-clojure-indent 'letk 1)
      (put-clojure-indent 'mlet 1)
      (put-clojure-indent 'when-letk 1)
      (put-clojure-indent 'go-loop 1)
      (put-clojure-indent 'this-as 'defun)
      (put-clojure-indent 'when-some '1)
      (put-clojure-indent 'if-some '1)
      (put-clojure-indent 'try+ 0)
      (put 'specify 'clojure-backtracking-indent '((2)))
      (put 'specify! 'clojure-backtracking-indent '((2)))
      (put 'defcomponent 'clojure-backtracking-indent '((2)))
      (put 'defcomponentk 'clojure-backtracking-indent '((2)))
      (put 'defmixin 'clojure-backtracking-indent '((2)))
      (put 'clojure.core/defrecord 'clojure-backtracking-indent '(4 4 (2)))
      (put 's/defrecord 'clojure-backtracking-indent '(4 4 (2)))
      (put 's/defrecord+ 'clojure-backtracking-indent '(4 4 (2)))
      (put 'potemkin/deftype+ 'clojure-backtracking-indent '(4 4 (2)))
      (put 'potemkin/defrecord+ 'clojure-backtracking-indent '(4 4 (2)))
      )

    (defun my/clojure-mode-hook ()
      (add-hook 'before-save-hook 'my/cleanup-buffer nil t)
      (clj-refactor-mode 1)
      (paredit-mode 1)
      (rainbow-delimiters-mode 1)
      (my/clojure-mode-custom-indent)
      ;; (prettify-symbols-mode 1)
      )

    (add-hook 'clojure-mode-hook #'yas-minor-mode)
    (add-hook 'clojure-mode-hook 'my/clojure-mode-hook)))

;;; setup-clojure-mode.el ends here
