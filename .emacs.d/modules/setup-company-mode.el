(use-package company
  :defer t
  :config
  (global-company-mode +1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)

  (add-to-list 'company-backends '(company-capf :with company-dabbrev-code))

  (bind-keys :map global-map
             ("C-i" . company-complete))

  (bind-keys :map company-active-map
             ("C-i" . company-complete-selection)
             ("C-p" . company-select-previous-or-abort)
             ("C-n" . company-select-next-or-abort)
             ("C-d" . company-show-doc-buffer)
             ("C-h" . nil))

  (use-package helm-company
    :config
    (bind-keys :map company-active-map
               ("C-s" . helm-company))))
