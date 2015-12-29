(use-package e2wm
  :config
  (progn
    (global-set-key (kbd "M-+") 'e2wm:start-management)

    (setq e2wm:c-max-history-num 1000)

    (setq e2wm:c-code-winfo
          '((:name main)
            (:name files :plugin files)
            (:name history :plugin history-list)
            (:name sub :buffer "*info*" :default-hide nil)
            (:name imenu :plugin imenu :default-hide t)))

    (setq e2wm:c-code-recipe
          '(| (:left-max-size 30)
              history
              (- (:upper-size-ratio 0.8)
                 main
                 sub)))

    (e2wm:add-keymap
     e2wm:pst-minor-mode-keymap
     '(("<s-left>" . e2wm:dp-code) ; codeへ変更
       ("<s-right>"  . e2wm:dp-two)  ; twoへ変更
       ("<s-up>"    . e2wm:dp-doc)  ; docへ変更
       ("<s-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
       ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
       ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
       ("prefix L"  . ielm)
       ("C-M-i" . e2wm:dp-code-imenu-toggle-command)
       ("C-M-m" . e2wm:dp-code-main-maximize-toggle-command)
       ("M-m"       . e2wm:pst-window-select-main-command)
       ) e2wm:prefix-key)

    (e2wm:start-management)))
