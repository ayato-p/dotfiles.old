;; Performance
(setq gc-cons-threshold 20000000)
(setq jit-lock-stealth-time 16
      jit-lock-defer-contextually t
      jit-lock-stealth-nice 0.5)
(setq-default font-lock-multiline t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show active region
(transient-mark-mode 1)

;; auto pair
;; (electric-pair-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'right)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; No lock files
(setq create-lockfiles nil)

(setq next-screen-context-lines 5)

(use-package smart-newline
  :config
  (add-hook 'clojure-mode-hook 'smart-newline-mode)
  (add-hook 'org-mode-hook 'smart-newline-mode)
  (defun my/smart-newline (of &rest arglist)
    (apply (cond (current-prefix-arg #'newline)
                 (t of))
           arglist))
  (advice-add 'smart-newline :around #'my/smart-newline))

(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; anzu
(use-package anzu
  :config (global-anzu-mode 1))

;; recentf
(use-package recentf
  :config
  (progn
    (recentf-mode 1)
    (setq recentf-max-menu-items 50)))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
     (defun my-dired-find-marked-files (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

(use-package ace-isearch
  :config
  (global-ace-isearch-mode 1))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; (use-package helm-wordnet
;;   :config (setq helm-wordnet-wordnet-location "/usr/share/wordnet"))

(use-package helm-swoop
  :config
  (progn
    (setq helm-swoop-split-with-multiple-windows t)))

;; editor config
(setq edconf-exec-path "/usr/bin/editorconfig")

(setq visible-bell t
      font-lock-maximum-decoration t
      truncate-partial-width-windows nil
      ;; disable to buckup funciton
      backup-inhibited t
      delete-auto-save-files t
      ;; completion ignore case (lower/upper)
      completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-message t)

(savehist-mode 1)
(column-number-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)

(use-package paren
  :init (progn (setq show-paren-style 'parenthesis)
               (show-paren-mode 1)))

(use-package rainbow-mode
  :init (progn (add-hook 'css-mode-hook 'rainbow-mode)
               (add-hook 'clojure-mode-hook 'rainbow-mode)
               (add-hook 'js2-mode-hook 'rainbow-mode)
               (add-hook 'web-mode-hook 'rainbow-mode)
               (add-hook 'emacs-lisp-mode 'rainbow-mode)))

(use-package eww
  :init
  (setq eww-search-prefix "https://www.google.co.jp/search?q="))

(use-package emojify
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package emoji-cheat-sheet-plus
  :defer t
  :init
  (progn
    ;; enabled emoji in buffer
    (add-hook 'org-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    ;; (add-hook 'text-mode-hook 'emoji-cheat-sheet-plus-display-mode)
    ;; insert emoji with helm
    (bind-keys :map global-map
               ("C-c C-e" . emoji-cheat-sheet-plus-insert))))

(use-package image
  :defer t
  :config
  (add-hook 'image-mode-hook
            (lambda ()
              (use-package image+
                :config
                (use-package smartrep
                  :config
                  (smartrep-define-key
                      image-mode-map "C-x" '(("-" . 'imagex-sticky-zoom-out)
                                             ("+" . 'imagex-sticky-zoom-in))))))))


(when window-system
  (load-theme 'zenburn t)
  ;; (load-theme 'hc-zenburn t)
  ;; (load-theme 'gruvbox t)
  ;; (load-theme 'ample t t)
  ;; (load-theme 'ample-flat t t)
  ;; (enable-theme 'ample-flat)

  (custom-set-faces
   '(e2wm:face-history-list-normal ((t (:foreground "LightGoldenrod1")))))

  ;; Font settings
  ;;  Font width checker:
  ;;    The both edges of two lines below should be aligned.
  ;;    |あいうえおかきくけこさしすせそ🍺|
  ;;    |''''''''''''''''''''''''''''''''|
  ;;
  (let* ((size 8)
         (asciifont "Dejavu Sans Mono")
         (jpfont "TakaoGothic")
         (emojifont "Dejavu Sans Mono")
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont))
         (emoji-fontspec (font-spec :family emojifont)))
    (set-face-attribute 'default nil :family asciifont :height (* size 10) :weight 'light)
    (setq face-font-rescale-alist nil)
    (add-to-list 'face-font-rescale-alist `(,jpfont . 1.2))
    (add-to-list 'face-font-rescale-alist `(,emojifont . 0.95))
    (set-fontset-font nil 'symbol emoji-fontspec nil)
    (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font nil 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font nil '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font nil '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
    )

  ;; (setq default-frame-alist '((font . "Dejavu Sans Mono-7.5")))
  ;; (set-default-font "Dejavu Sans Mono-7.5")
  ;; (set-face-font 'variable-pitch "Dejavu Sans Mono-7.5")
  ;; (set-fontset-font (frame-parameter nil 'font)
  ;;                   'japanese-jisx0208
  ;;                   '("Takaoゴシック" . "unicode-bmp"))

  (use-package server
    :config (unless (server-running-p)
              (server-start))))

(use-package hl-line
  :init
  (progn
    (global-hl-line-mode -1)
    (set-face-background 'hl-line "#525252")))

(use-package skk
  :config
  (setq default-input-method "japanese-skk")
  (setq skk-kuten-touten-alist
        (cons '(my-en "." . ",")
              skk-kuten-touten-alist))
  (setq-default skk-kutouten-type 'jp)

  :init
  (use-package skk-decor)
  (setq skk-kakutei-when-unique-candidate nil)
  (setq skk-egg-like-newline t)
  (setq skk-isearch-mode-enable nil)
  (setq skk-show-inline 'vertical)
  (setq skk-cdb-large-jisyo "/usr/share/skk/SKK-JISYO.LL.cdb")
  (setq skk-sticky-key ":")
  (setq skk-auto-insert-paren nil)
  (add-to-list 'context-skk-programming-mode 'clojure-mode)

  (add-hook 'skk-load-hook
            (lambda ()
              (require 'context-skk)))

  (add-hook 'find-file-hook
            (lambda ()
              (skk-latin-mode t)))

  ;; conflict skk && paredit
  (defun paredit-newline\\skk-kakutei (origfun &rest arglist)
    (apply (cond ((not skk-mode) origfun)
                 (t #'skk-kakutei))
           arglist))

  (advice-add 'paredit-newline :around #'paredit-newline\\skk-kakutei)

  (defun paredit-newline\\skk-insert (origfun &rest arglist)
    (apply (cond (skk-j-mode #'skk-insert)
                 (t origfun))
           arglist))

  (advice-add 'cljr-slash :around #'paredit-newline\\skk-insert)

  ;; (setq skk-rom-kana-base-rule-list
  ;;       '(("a" nil ("ア" . "あ"))
  ;;         ("nn" "n" ("ッ" . "っ"))
  ;;         ("na" nil ("バ" . "ば"))
  ;;         ("nd" nil ("ベ" . "べ"))
  ;;         ("ng" nil ("ビ" . "び"))
  ;;         ("ns" nil ("ボ" . "ぼ"))
  ;;         ("nf" nil ("ブ" . "ぶ"))
  ;;         ("nta" nil ("ビャ" . "びゃ"))
  ;;         ("ntd" nil ("ビェ" . "びぇ"))
  ;;         ("ntg" nil ("ビィ" . "びぃ"))
  ;;         ("nts" nil ("ビョ" . "びょ"))
  ;;         ("ntf" nil ("ビュ" . "びゅ"))
  ;;         ("ii" "i" ("ッ" . "っ"))
  ;;         ("ija" nil ("チャ" . "ちゃ"))
  ;;         ("ijd" nil ("チェ" . "ちぇ"))
  ;;         ("ijg" nil ("チ" . "ち"))
  ;;         ("ijs" nil ("チョ" . "ちょ"))
  ;;         ("ijf" nil ("チュ" . "ちゅ"))
  ;;         ("ita" nil ("チャ" . "ちゃ"))
  ;;         ("itd" nil ("チェ" . "ちぇ"))
  ;;         ("itg" nil ("チィ" . "ちぃ"))
  ;;         ("its" nil ("チョ" . "ちょ"))
  ;;         ("itf" nil ("チュ" . "ちゅ"))
  ;;         ("hh" "h" ("ッ" . "っ"))
  ;;         ("ha" nil ("ダ" . "だ"))
  ;;         ("hd" nil ("デ" . "で"))
  ;;         ("hja" nil ("デャ" . "でゃ"))
  ;;         ("hjd" nil ("デェ" . "でぇ"))
  ;;         ("hjg" nil ("ディ" . "でぃ"))
  ;;         ("hjs" nil ("デョ" . "でょ"))
  ;;         ("hjf" nil ("デュ" . "でゅ"))
  ;;         ("hg" nil ("ヂ" . "ぢ"))
  ;;         ("hs" nil ("ド" . "ど"))
  ;;         ("hf" nil ("ヅ" . "づ"))
  ;;         ("hta" nil ("ヂャ" . "ぢゃ"))
  ;;         ("htd" nil ("ヂェ" . "ぢぇ"))
  ;;         ("htg" nil ("ヂィ" . "ぢぃ"))
  ;;         ("hts" nil ("ヂョ" . "ぢょ"))
  ;;         ("htf" nil ("ヂュ" . "ぢゅ"))
  ;;         ("d" nil ("エ" . "え"))
  ;;         ("yy" "y" ("ッ" . "っ"))
  ;;         ("ya" nil ("ファ" . "ふぁ"))
  ;;         ("yd" nil ("フェ" . "ふぇ"))
  ;;         ("yg" nil ("フィ" . "ふぃ"))
  ;;         ("ys" nil ("フォ" . "ふぉ"))
  ;;         ("yf" nil ("フ" . "ふ"))
  ;;         ("yta" nil ("フャ" . "ふゃ"))
  ;;         ("ytd" nil ("フェ" . "ふぇ"))
  ;;         ("ytg" nil ("フィ" . "ふぃ"))
  ;;         ("yts" nil ("フョ" . "ふょ"))
  ;;         ("ytf" nil ("フュ" . "ふゅ"))
  ;;         ("uu" "u" ("ッ" . "っ"))
  ;;         ("ua" nil ("ガ" . "が"))
  ;;         ("ud" nil ("ゲ" . "げ"))
  ;;         ("ug" nil ("ギ" . "ぎ"))
  ;;         ("us" nil ("ゴ" . "ご"))
  ;;         ("uf" nil ("グ" . "ぐ"))
  ;;         ("uta" nil ("ギャ" . "ぎゃ"))
  ;;         ("utd" nil ("ギェ" . "ぎぇ"))
  ;;         ("utg" nil ("ギィ" . "ぎぃ"))
  ;;         ("uts" nil ("ギョ" . "ぎょ"))
  ;;         ("utf" nil ("ギュ" . "ぎゅ"))
  ;;         ("ja" nil ("ハ" . "は"))
  ;;         ("jd" nil ("ヘ" . "へ"))
  ;;         ("jg" nil ("ヒ" . "ひ"))
  ;;         ("js" nil ("ホ" . "ほ"))
  ;;         ("jf" nil ("フ" . "ふ"))
  ;;         ("jta" nil ("ヒャ" . "ひゃ"))
  ;;         ("jtd" nil ("ヒェ" . "ひぇ"))
  ;;         ("jtg" nil ("ヒィ" . "ひぃ"))
  ;;         ("jts" nil ("ヒョ" . "ひょ"))
  ;;         ("jtf" nil ("ヒュ" . "ひゅ"))
  ;;         ("g" nil ("イ" . "い"))
  ;;         ("cc" "c" ("ッ" . "っ"))
  ;;         ("ca" nil ("ジャ" . "じゃ"))
  ;;         ("cd" nil ("ジェ" . "じぇ"))
  ;;         ("cg" nil ("ジ" . "じ"))
  ;;         ("cs" nil ("ジョ" . "じょ"))
  ;;         ("cf" nil ("ジュ" . "じゅ"))
  ;;         ("cta" nil ("ジャ" . "じゃ"))
  ;;         ("ctd" nil ("ジェ" . "じぇ"))
  ;;         ("ctg" nil ("ジィ" . "じぃ"))
  ;;         ("cts" nil ("ジョ" . "じょ"))
  ;;         ("ctf" nil ("ジュ" . "じゅ"))
  ;;         ("vv" "v" ("ッ" . "っ"))
  ;;         ("va" nil ("カ" . "か"))
  ;;         ("vd" nil ("ケ" . "け"))
  ;;         ("vg" nil ("キ" . "き"))
  ;;         ("vs" nil ("コ" . "こ"))
  ;;         ("vf" nil ("ク" . "く"))
  ;;         ("ia" nil ("カ" . "か"))
  ;;         ("id" nil ("ケ" . "け"))
  ;;         ("ig" nil ("キ" . "き"))
  ;;         ("is" nil ("コ" . "こ"))
  ;;         ("if" nil ("ク" . "く"))
  ;;         ("vta" nil ("キャ" . "きゃ"))
  ;;         ("vtd" nil ("キェ" . "きぇ"))
  ;;         ("vtg" nil ("キィ" . "きぃ"))
  ;;         ("vts" nil ("キョ" . "きょ"))
  ;;         ("vtf" nil ("キュ" . "きゅ"))
  ;;         ("ma" nil ("マ" . "ま"))
  ;;         ("md" nil ("メ" . "め"))
  ;;         ("mg" nil ("ミ" . "み"))
  ;;         ("ms" nil ("モ" . "も"))
  ;;         ("mf" nil ("ム" . "む"))
  ;;         ("mta" nil ("ミャ" . "みゃ"))
  ;;         ("mtd" nil ("ミェ" . "みぇ"))
  ;;         ("mtg" nil ("ミィ" . "みぃ"))
  ;;         ("mts" nil ("ミョ" . "みょ"))
  ;;         ("mtf" nil ("ミュ" . "みゅ"))
  ;;         ("l" nil ("ン" . "ん"))
  ;;         ("lq" nil ("ン" . "ん"))
  ;;         ("la" nil ("ナ" . "な"))
  ;;         ("ld" nil ("ネ" . "ね"))
  ;;         ("lg" nil ("ニ" . "に"))
  ;;         ("ll" nil ("ン" . "ん"))
  ;;         ("ls" nil ("ノ" . "の"))
  ;;         ("lf" nil ("ヌ" . "ぬ"))
  ;;         ("lta" nil ("ニャ" . "にゃ"))
  ;;         ("ltd" nil ("ニェ" . "にぇ"))
  ;;         ("ltg" nil ("ニィ" . "にぃ"))
  ;;         ("lts" nil ("ニョ" . "にょ"))
  ;;         ("ltf" nil ("ニュ" . "にゅ"))
  ;;         ("s" nil ("オ" . "お"))
  ;;         ("rr" "r" ("ッ" . "っ"))
  ;;         ("ra" nil ("パ" . "ぱ"))
  ;;         ("rd" nil ("ペ" . "ぺ"))
  ;;         ("rg" nil ("ピ" . "ぴ"))
  ;;         ("rs" nil ("ポ" . "ぽ"))
  ;;         ("rf" nil ("プ" . "ぷ"))
  ;;         ("rta" nil ("ピャ" . "ぴゃ"))
  ;;         ("rtd" nil ("ピェ" . "ぴぇ"))
  ;;         ("rtg" nil ("ピィ" . "ぴぃ"))
  ;;         ("rts" nil ("ピョ" . "ぴょ"))
  ;;         ("rtf" nil ("ピュ" . "ぴゅ"))
  ;;         ("oo" "o" ("ッ" . "っ"))
  ;;         ("oa" nil ("ラ" . "ら"))
  ;;         ("od" nil ("レ" . "れ"))
  ;;         ("og" nil ("リ" . "り"))
  ;;         ("os" nil ("ロ" . "ろ"))
  ;;         ("of" nil ("ル" . "る"))
  ;;         ("ota" nil ("リャ" . "りゃ"))
  ;;         ("otd" nil ("リェ" . "りぇ"))
  ;;         ("otg" nil ("リィ" . "りぃ"))
  ;;         ("ots" nil ("リョ" . "りょ"))
  ;;         ("otf" nil ("リュ" . "りゅ"))
  ;;         ("::" ":" ("ッ" . "っ"))
  ;;         (":a" nil ("サ" . "さ"))
  ;;         (":d" nil ("セ" . "せ"))
  ;;         (":ja" nil ("シャ" . "しゃ"))
  ;;         (":jd" nil ("シェ" . "しぇ"))
  ;;         (":jg" nil ("シ" . "し"))
  ;;         (":js" nil ("ショ" . "しょ"))
  ;;         (":jf" nil ("シュ" . "しゅ"))
  ;;         (":g" nil ("シ" . "し"))
  ;;         (":s" nil ("ソ" . "そ"))
  ;;         (":f" nil ("ス" . "す"))
  ;;         (":ta" nil ("シャ" . "しゃ"))
  ;;         (":td" nil ("シェ" . "しぇ"))
  ;;         (":tg" nil ("シィ" . "しぃ"))
  ;;         (":ts" nil ("ショ" . "しょ"))
  ;;         (":tf" nil ("シュ" . "しゅ"))
  ;;         ("kk" "k" ("ッ" . "っ"))
  ;;         ("ka" nil ("タ" . "た"))
  ;;         ("kd" nil ("テ" . "て"))
  ;;         ("kja" nil ("テァ" . "てぁ"))
  ;;         ("kjd" nil ("テェ" . "てぇ"))
  ;;         ("kjg" nil ("ティ" . "てぃ"))
  ;;         ("kjs" nil ("テョ" . "てょ"))
  ;;         ("kjf" nil ("テュ" . "てゅ"))
  ;;         ("kg" nil ("チ" . "ち"))
  ;;         ("ks" nil ("ト" . "と"))
  ;;         ("k:f" nil ("ツ" . "つ"))
  ;;         ("kf" nil ("ツ" . "つ"))
  ;;         ("kta" nil ("チャ" . "ちゃ"))
  ;;         ("ktd" nil ("チェ" . "ちぇ"))
  ;;         ("ktg" nil ("チィ" . "ちぃ"))
  ;;         ("kts" nil ("チョ" . "ちょ"))
  ;;         ("ktf" nil ("チュ" . "ちゅ"))
  ;;         ("f" nil ("ウ" . "う"))
  ;;         (".." "." ("ッ" . "っ"))
  ;;         (".a" nil ("ヴァ" . "う゛ぁ"))
  ;;         (".d" nil ("ヴェ" . "う゛ぇ"))
  ;;         (".g" nil ("ヴィ" . "う゛ぃ"))
  ;;         (".s" nil ("ヴォ" . "う゛ぉ"))
  ;;         (".f" nil ("ヴ" . "う゛"))
  ;;         (",," "," ("ッ" . "っ"))
  ;;         (",a" nil ("ワ" . "わ"))
  ;;         (",d" nil ("ウェ" . "うぇ"))
  ;;         (",g" nil ("ウィ" . "うぃ"))
  ;;         (",s" nil ("ヲ" . "を"))
  ;;         (",f" nil ("ウ" . "う"))
  ;;         ("bb" "b" ("ッ" . "っ"))
  ;;         ("ba" nil ("ァ" . "ぁ"))
  ;;         ("bd" nil ("ェ" . "ぇ"))
  ;;         ("bg" nil ("ィ" . "ぃ"))
  ;;         ("bva" nil ("ヵ" . "か"))
  ;;         ("bvd" nil ("ヶ" . "け"))
  ;;         ("bs" nil ("ォ" . "ぉ"))
  ;;         ("bk:f" nil ("ッ" . "っ"))
  ;;         ("bkf" nil ("ッ" . "っ"))
  ;;         ("bf" nil ("ゥ" . "ぅ"))
  ;;         ("b,a" nil ("ヮ" . "ゎ"))
  ;;         ("b,d" nil ("ヱ" . "ゑ"))
  ;;         ("b,g" nil ("ヰ" . "ゐ"))
  ;;         ("bta" nil ("ャ" . "ゃ"))
  ;;         ("bts" nil ("ョ" . "ょ"))
  ;;         ("btf" nil ("ュ" . "ゅ"))
  ;;         ("tt" "t" ("ッ" . "っ"))
  ;;         ("ta" nil ("ヤ" . "や"))
  ;;         ("td" nil ("イェ" . "いぇ"))
  ;;         ("ts" nil ("ヨ" . "よ"))
  ;;         ("tf" nil ("ユ" . "ゆ"))
  ;;         ("//" "/" ("ッ" . "っ"))
  ;;         ("/a" nil ("ザ" . "ざ"))
  ;;         ("/d" nil ("ゼ" . "ぜ"))
  ;;         ("/g" nil ("ジ" . "じ"))
  ;;         ("/s" nil ("ゾ" . "ぞ"))
  ;;         ("/f" nil ("ズ" . "ず"))
  ;;         ("/ta" nil ("ジャ" . "じゃ"))
  ;;         ("/td" nil ("ジェ" . "じぇ"))
  ;;         ("/tg" nil ("ジィ" . "じぃ"))
  ;;         ("/ts" nil ("ジョ" . "じょ"))
  ;;         ("/tf" nil ("ジュ" . "じゅ"))

  ;;         ("z " nil "　")
  ;;         ("z*" nil "※")
  ;;         ("z," nil "‥")
  ;;         ("z-" nil "〜")
  ;;         ("z." nil "…")
  ;;         ("z/" nil "・")
  ;;         ("z0" nil "○")
  ;;         ("z:" nil "゜")
  ;;         ("z;" nil "゛")
  ;;         ("z@" nil "◎")
  ;;         ("z[" nil "『")
  ;;         ("z]" nil "』")
  ;;         ("z{" nil "【")
  ;;         ("z}" nil "】")
  ;;         ("z(" nil "（")
  ;;         ("z)" nil "）")
  ;;         ("zh" nil "←")
  ;;         ("zj" nil "↓")
  ;;         ("zk" nil "↑")
  ;;         ("zl" nil "→")
  ;;         ("zL" nil "⇒")
  ;;         (":" nil "：")
  ;;         (";" nil "；")
  ;;         ("?" nil "？")
  ;;         ("[" nil "「")
  ;;         ("]" nil "」")
  ;;         ("." nil skk-auto-kutouten)
  ;;         ("," nil skk-auto-kutouten)
  ;;         ("-" nil skk-auto-kutouten)
  ;;         ("p" nil skk-latin-mode)
  ;;         ("q" nil skk-toggle-kana)
  ;;         ("P" nil skk-jisx0208-latin-mode)
  ;;         ("Q" nil skk-set-henkan-point-subr)
  ;;         ("X" nil skk-purge-from-jisyo)
  ;;         ("/" nil skk-abbrev-mode)
  ;;         ("$" nil skk-display-code-for-char-at-point)
  ;;         ("@" nil skk-today)
  ;;         ("\\" nil skk-input-by-code-or-menu)))
  )

;; for whitespace
(use-package whitespace
  :init
  (progn
    (setq whitespace-style '(face
                             trailing
                             tabs
                             spaces
                             empty
                             space-mark
                             tab-mark
                             lines-tail))
    (setq whitespace-space-regexp "\\(\x3000+\\)")
    (setq whitespace-display-mappings
          '((space-mark ?\u3000 [?\u25a1])
            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
    (global-whitespace-mode 1)

    (set-face-attribute 'whitespace-trailing nil
                        :background "tomato"
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :background "deep sky blue"
                        :underline t)
    (set-face-attribute 'whitespace-space nil
                        :background "green"
                        :weight 'bold)
    (set-face-attribute 'whitespace-empty nil
                        :background "#313131")
    (setq-default tab-width 4 indent-tabs-mode nil)

    ;; (setq whitespace-line-column 100) ;; limit line length
    (setq whitespace-line-column 70) ;; for 執筆
    (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package po-mode
  :config
  (bind-keys :map po-mode-map
             ("j" . po-msgid-to-msgstr)))

;; git gutter
(use-package git-gutter
  :config
  (progn
    (global-git-gutter-mode +1)
    ;; update-commands に e2wm のコマンドを入れていないと更新されない問題?
    (add-to-list 'git-gutter:update-commands 'e2wm:pst-history-forward-command)
    (add-to-list 'git-gutter:update-commands 'e2wm:pst-history-back-command)
    (set-face-foreground 'git-gutter:modified "black")
    (set-face-background 'git-gutter:modified "white")
    (set-face-foreground 'git-gutter:added "black")
    (set-face-background 'git-gutter:added "dark orange")
    (set-face-foreground 'git-gutter:deleted "black")
    (set-face-background 'git-gutter:deleted "blue")
    (custom-set-variables
     '(git-gutter:update-interval 1)
     '(git-gutter:window-width 2)
     '(git-gutter:modified-sign "❄")
     '(git-gutter:added-sign "☀")
     '(git-gutter:deleted-sign "☂"))))

(prefer-coding-system 'utf-8)
(setq ruby-insert-encoding-magic-comment nil)
(set-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8-unix)

(use-package edit-server
  :config
  (edit-server-start))
