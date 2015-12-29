;;; core-loader.el --- core-loader

;;; Commentary:

;;; Code:

(defvar my/modules-dir (locate-user-emacs-file "modules"))
(defvar my/modules nil)

(defun my/module-selector (modules)
  "Sort and filter given MODULES according to `my/modules'."
  (delq nil
        (mapcar (lambda (module)
                  (let ((elc (concat module ".elc"))
                        (el (concat module ".el")))
                    (cond ((member elc modules) elc)
                          ((member el modules) el)
                          ((member module modules) module)
                          (t
                           (message "Unknown module: %s" module)
                           nil))))
                (delq nil my/modules))))

(defun my/load-modules ()
  "Load modules."
  (setq init-loader-show-log-after-init t
        init-loader-byte-compile t
        init-loader-default-regexp "\\`\\(?:setup-\\|utils\\|custom.el\\)"
        init-loader-sort-function 'my/module-selector)

  (init-loader-load my/modules-dir))

;; ;; Define helm source
;; (use-package helm-files
;;   :bind ("C-x r ." . my/helm-list-emacs-modules)
;;   :config (progn
;;             (defvar my/helm-source-emacs-modules
;;               `((name . "EmacsModules")
;;                 (candidates . (lambda ()
;;                                 (with-helm-current-buffer
;;                                   (let ((dirs (append (directory-files user-emacs-directory t)
;;                                                       (directory-files my/modules-dir t)))
;;                                         (pred (lambda (d) (string-match "^\\(?:.*\.el\\|.*[^\.]Cask\\)$" d))))
;;                                     (-filter pred dirs)))))
;;                 (type . file)))

;;             (defun my/helm-list-emacs-modules ()
;;               (interactive)
;;               (helm-other-buffer 'my/helm-source-emacs-modules "*helm-emacs-modules*"))))

(provide 'core-loader)
;;; core-loader.el ends here
