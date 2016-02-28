(use-package company
  :defer t
  :config
  (global-company-mode +1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)

  (add-to-list 'company-backends
               '(company-capf :with company-dabbrev-code
                              :with company-yasnippet))

  (bind-keys :map global-map
             ("C-i" . company-indent-or-complete-common))

  (bind-keys :map company-active-map
             ("C-i" . company-complete-common)
             ("C-p" . company-select-previous-or-abort)
             ("C-n" . company-select-next-or-abort)
             ("C-h" . nil))

  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous))

  (use-package company-quickhelp
    :config
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay nil)
    (bind-keys :map company-active-map
               ("C-d" . company-quickhelp-manual-begin)))

  ;; (use-package helm-company
  ;;   :config
  ;;   (bind-keys :map company-active-map
  ;;              ("C-s" . helm-company)))

  (defun my/toggle-company-ispell ()
    (interactive)
    (cond
     ((memq 'company-ispell company-backends)
      (setq company-backends (delete 'company-ispell company-backends))
      (message "company-ispell disabled"))
     (t
      (add-to-list 'company-backends 'company-ispell)
      (message "company-ispell enabled!"))))
  )
