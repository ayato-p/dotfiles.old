(use-package open-junk-file
  :config
  (bind-keys :map global-map
             ("C-x j" . open-junk-file))
  (setq open-junk-file-format "~/memo/junk/%Y-%m%d-%H%M%S."))

(use-package org
  :config

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

  (setq org-edit-src-content-indentation 0
        org-src-tab-acts-natively t
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)
  )
