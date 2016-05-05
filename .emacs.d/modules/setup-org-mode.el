(use-package open-junk-file
  :config
  (bind-keys :map global-map
             ("C-x j" . open-junk-file))
  (setq open-junk-file-format "~/memo/junk/%Y-%m%d-%H%M%S."))

(use-package org
  :config
  (progn


    (setq org-directory "~/memo/junk")
    (setq my/notebook-directory "~/notebook")
    (setq org-agenda-files (list org-directory my/notebook-directory))

    (setq org-edit-src-content-indentation 0
          org-src-tab-acts-natively t
          org-src-fontify-natively t
          org-confirm-babel-evaluate nil)

    (setq org-pandoc-menu-entry
          '(
            ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
            (?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
            (?A "as asciidoc." org-pandoc-export-as-asciidoc)
            ;;(?h "to html5." org-pandoc-export-to-html5)
            (?h "to html5 and open." org-pandoc-export-to-html5-and-open)
            (?H "as html5." org-pandoc-export-as-html5)
            ;;(?g "to markdown_github." org-pandoc-export-to-markdown_github)
            (?g "to markdown_github and open." org-pandoc-export-to-markdown_github-and-open)
            (?G "as markdown_github." org-pandoc-export-as-markdown_github)
            ;;(?v "to revealjs." org-pandoc-export-to-revealjs)
            (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
            (?V "as revealjs." org-pandoc-export-as-revealjs)
            ;;(?: "to rst." org-pandoc-export-to-rst)
            (?r "to rst and open." org-pandoc-export-to-rst-and-open)
            (?R "as rst." org-pandoc-export-as-rst)))

    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline (concat my/notebook-directory "/todo.org") "Tasks")
             "* TODO %?\n %i\n %a")
            ("d" "Discussion" entry (file+headline (concat my/notebook-directory "/discussion.org") "Discussion")
             "%[~/notebook/templates/discussion.txt]")
            ("j" "Journal" entry (file+datetree (concat my/notebook-directory "/journal.org"))
             "* %?\n %U\n %i\n %a")
            ("n" "Note" entry (file+headline (concat my/notebook-directory "/notes.org") "Notes")
             "* %?\n %U\n %i")))

    (with-eval-after-load 'ox
      (require 'ox-pandoc)
      (add-hook 'org-pandoc-after-processing-rst-hook
                (lambda (&optional _)
                  (interactive)
                  (while (re-search-forward "^\\.\\.\scode::" nil t)
                    (replace-match ".. sourcecode::")))))

    (use-package org-tree-slide
      :config
      (progn
        (bind-keys :map org-mode-map
                   ("<f8>" . org-tree-slide-mode))
        (bind-keys :map org-tree-slide-mode-map
                   ("<right>" . org-tree-slide-move-next-tree)
                   ("<left>" . org-tree-slide-move-previous-tree))
        (org-tree-slide-simple-profile)))

    (use-package company
      :init
      (add-hook 'org-mode-hook #'company-mode)
      (defun add-pcomplete-to-capf ()
        (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
      (add-hook 'org-mode-hook #'add-pcomplete-to-capf))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (scheme . t)
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
               ("C-x C-o" . my/org-list)
               ("C-c c" . org-capture))))
