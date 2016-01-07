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

;; $BJ8=q%P%C%U%!$N"'%b!<%I$K$*$1$kJQ498uJd$NI=<($rAu>~$9$k$K$O(B
;; skk-henkan-face $B$r@_Dj$7$^$9!#(B
;;
;; $B%$%s%i%$%s!"(Btooltip $BKt$O(B $B8uJd%P%C%U%!$NJQ498uJd$NI=<($rAu>~$9$k$K$O(B
;; skk-treat-candidate-appearance-function $B$r@_Dj$7$^$9!#(B
;; $B$7$+$7!"(Bskk-treat-candidate-appearance-function $B$N@_Dj$O!"(B
;;   o $BJ8=q%P%C%U%!$N"'%b!<%I$K$*$1$kJQ498uJd$NI=<($K$b1F6A(B
;;   o $B%$%s%i%$%s!"(Btooltip $BKt$O(B $B8uJd%P%C%U%!$N#3$D$NA4$F$K1F6A(B
;; $B$H$$$&8z2L$b$"$j$^$9!#(B
;;
;; $B$=$N$?$a!"Nc$($P!"!V"'%b!<%I$K1F6A$;$:!"8uJd%P%C%U%!$K8B$C$?I=<($NAu>~!W(B
;; $B$r$7$?$$J}$N$?$a$K(B advice $B$r:n$C$F$_$^$7$?!#(B

;;
;;  ~/.skk $B$K$F(B
;;      (require skk-decor)
;;  $B$H@_Dj$7$F$/$@$5$$!#(B

;;; Face:

(defface skk-decor-cand-face
  '(
    ;; (((type tty))
    ;;  (:foreground "light sea green" :background "dark sea green"))
    (t
     (:foreground "#DCDCCC" :background "#656555"
                  ;; :box (:line-width 3 :color "gray40" :style released-button)
                  )))
  "$B8uJd$KE,MQ$9$k(B FACE"
  :group 'skk-visual)

(defface skk-decor-anno-face
  '((((type tty))
     (:foreground "DarkSeaGreen"))
    (t
     (:foreground "yellow" :height 120)))
  "$B%"%N%F!<%7%g%s$KE,MQ$9$k(B FACE"
  :group 'skk-visual)

;;; Code:

(defadvice skk-inline-show-vertically (before skk-decor (string face) activate)
  "skk-show-inline 'vertical $B$K8B$C$F%U%'%$%9$r:nMQ$5$;$k(B."
  (set-face-attribute 'skk-decor-cand-face nil ; $B%U%'%$%9$NHyD4@0(B
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
  "tooltip $B$K8B$C$F%U%'%$%9$r:nMQ$5$;$k(B."
  (when (null (eq situation 'annotation))
    (set-face-attribute 'skk-decor-cand-face nil ; $B%U%'%$%9$NHyD4@0(B
                        :family "$B2V1`L@D+(BA"  ;
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
  "$B8uJd%P%C%U%!$K8B$C$F%U%'%$%9$r:nMQ$5$;$k(B."
  (set-face-attribute 'skk-decor-cand-face nil ; $B%U%'%$%9$NHyD4@0(B
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
