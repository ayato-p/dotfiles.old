(use-package open-junk-file
  :config
  (bind-keys :map global-map
             ("C-x j" . open-junk-file))
  (setq open-junk-file-format "~/memo/junk/%Y-%m%d-%H%M%S."))

(use-package org
  :config

  (setq org-directory "~/memo/junk")
  (setq org-agenda-files (list org-directory))

  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)

  (use-package company
    :init
    (add-hook 'org-mode-hook #'company-mode)
    (defun add-pcomplete-to-capf ()
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
    (add-hook 'org-mode-hook #'add-pcomplete-to-capf))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)))

  (use-package ob-clojure
    :config
    (setq org-babel-clojure-backend 'cider))


  ;; my own functions
  (defun my/org-list ()
    (interactive)
    (let ((of (helm :sources (helm-build-sync-source "Org files"
                               :candidates (reverse (directory-files org-directory))
                               :fuzzy-match t)
                    :buffer "*org files list*")))
      (when of
        (find-file (concat org-directory "/" of)))))

  (bind-keys :map global-map
             ("C-x C-o" . my/org-list)))
