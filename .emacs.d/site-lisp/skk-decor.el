;;; skk-decor.el --- decorate the candidate for SKK -*- mode: emacs-lisp; coding: iso-2022-jp -*-

;; Copyright (C) 2014 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>

;; Author: 2014 Tsuyoshi Kitamoto  <tsuyoshi.kitamoto@gmail.com>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-decor.el $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2014/03/12 21:39:22 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 文書バッファの▼モードにおける変換候補の表示を装飾するには
;; skk-henkan-face を設定します。
;;
;; インライン、tooltip 又は 候補バッファの変換候補の表示を装飾するには
;; skk-treat-candidate-appearance-function を設定します。
;; しかし、skk-treat-candidate-appearance-function の設定は、
;;   o 文書バッファの▼モードにおける変換候補の表示にも影響
;;   o インライン、tooltip 又は 候補バッファの３つの全てに影響
;; という効果もあります。
;;
;; そのため、例えば、「▼モードに影響せず、候補バッファに限った表示の装飾」
;; をしたい方のために advice を作ってみました。

;;
;;  ~/.skk にて
;;      (require skk-decor)
;;  と設定してください。

;;; Face:

(defface skk-decor-cand-face
  '(
    ;; (((type tty))
    ;;  (:foreground "light sea green" :background "dark sea green"))
    (t
     (:foreground "#DCDCCC" :background "#656555"
                  ;; :box (:line-width 3 :color "gray40" :style released-button)
                  )))
  "候補に適用する FACE"
  :group 'skk-visual)

(defface skk-decor-anno-face
  '((((type tty))
     (:foreground "DarkSeaGreen"))
    (t
     (:foreground "yellow" :height 120)))
  "アノテーションに適用する FACE"
  :group 'skk-visual)

;;; Code:

(defadvice skk-inline-show-vertically (before skk-decor (string face) activate)
  "skk-show-inline 'vertical に限ってフェイスを作用させる."
  (set-face-attribute 'skk-decor-cand-face nil ; フェイスの微調整
                      :height 180)         ;

  (let* ((sp    (reverse (split-string string "\n")))
         (rest  (car sp))
         (cands (reverse (cdr sp))))
    (setq string
          (concat (mapconcat
                   (lambda (x)
                     (let* ((d (skk-treat-strip-note-from-word (substring x 2)))
                            (c (car d))
                            (n (cdr d)))
                       (concat (substring x 0 2)
                               (propertize c 'face 'skk-decor-cand-face)
                               (when n
                                 (concat " " (propertize n 'face 'skk-decor-anno-face))
                                 ))))
                   cands "\n")
                  "\n" rest))))


(defadvice skk-tooltip-show-at-point (before skk-decor (text situation) activate)
  "tooltip に限ってフェイスを作用させる."
  (when (null (eq situation 'annotation))
    (set-face-attribute 'skk-decor-cand-face nil ; フェイスの微調整
                        :family "花園明朝A"  ;
                        :height 180)         ;
    (set-face-attribute 'skk-decor-anno-face nil ;
                        :height 100)         ;

    (let* ((sp    (reverse (split-string text "\n")))
           (rest  (car sp))
           (cands (reverse (cdr sp))))
      (setq text
            (concat (mapconcat
                     (lambda (x)
                       (let* ((d (skk-treat-strip-note-from-word (substring x 2)))
                              (c (car d))
                              (n (cdr d)))
                         (concat (substring x 0 2)
                                 (propertize c 'face 'skk-decor-cand-face)
                                 (when n
                                   (concat " " (propertize n 'face 'skk-decor-anno-face))
                                   ))))
                     cands "\n")
                    "\n" rest)))))


(defadvice skk-henkan-show-candidates-buffer (before skk-decor (str keys) activate)
  "候補バッファに限ってフェイスを作用させる."
  (set-face-attribute 'skk-decor-cand-face nil ; フェイスの微調整
                      :height 250)         ;

  (let* ((cand (reverse (split-string str "  " t)))
         (rest (car cand))
         (cand (reverse (cdr cand))))
    (setq str
          (concat (mapconcat
                   (lambda (x)
                     (let* ((d (skk-treat-strip-note-from-word (substring x 2)))
                            (c (car d))
                            (n (cdr d)))
                       (concat (substring x 0 2) ; `A:'
                               (propertize c 'face 'skk-decor-cand-face)
                               (when n
                                 (concat " " (propertize n 'face 'skk-decor-anno-face))
                                 ))))
                   cand "  ")
                  "  " rest))))

(provide 'skk-decor)

;;; skk-decor.el ends here
