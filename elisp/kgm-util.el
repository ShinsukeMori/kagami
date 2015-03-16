;;;; Kgm-lib.el -- SLM Kana Kanji Converter -*- coding: euc-jp; -*-

;; Author: Hirokuni Maeta<koji@kmc.gr.jp>
;; Keywords: japanese

;; This file is part of KAGAMI

;;; Commentary:
;;


(defun list0-n (n)
  (loop for i from 0 to n collect i))


(defun between (n a b)
  (and (<= a n) (<= n b)))


;; これで、ベクタがリストになる
;; Emacs Lispのリファレンスマニュアルにも書いてある正当なやり方
(defun vector-to-list (vector)
  (append vector nil))


;; (kgm-with-keyword-binds args
;;   (var1 :key1) (var2 :key2)
;;   body)

;; 上のようなコードを下に示すようにするマクロ。

;; (let ((g123 args))
;;   (let ((var1 (getf g123 :key1))
;; 	   (var2 (getf g123 :key2)))
;;     body))

;; Emacs Lispのラムダパラメータには&keyがないので、
;; その代わりのつもり。
(defmacro kgm-with-keyword-binds (args clauses &rest body)
  (let ((g (gensym)))
    `(let ((,g ,args))
       (let ,(mapcar #'(lambda (clause)
			 `(,(car clause) (getf ,g ,(cadr clause))))
		     clauses)
	 ,@body))))


(defun hiragana->katakana (hiragana-string)
  (apply #'concat
	 (mapcar #'char-to-string
		 (mapcar #'japanese-katakana
			 (vector-to-list hiragana-string)))))


(defun string->hankaku (string)
  (apply #'concat
	 (mapcar #'(lambda (ch)
		     (if (stringp ch) ch (char-to-string ch)))
		 (mapcar #'japanese-hankaku (vector-to-list string)))))



(defun string->zenkaku (string)
  (apply #'concat
	 (mapcar #'(lambda (ch)
		     (if (stringp ch) ch (char-to-string ch)))
		 (mapcar #'japanese-zenkaku
			 (vector-to-list string)))))

(defun string-downcase (string)
  (apply #'concat
	 (mapcar #'char-to-string
		 (mapcar #'downcase
			 (vector-to-list string)))))

(defun string-upcase (string)
  (apply #'concat
	 (mapcar #'char-to-string
		 (mapcar #'upcase
			 (vector-to-list string)))))

(defun string-capitalize (string)
  (if (string-equal string "")
      ""
    (concat (string-upcase (subseq string 0 1))
	    (string-downcase (subseq string 1)))))


(defun string-zenkaku-downcase (string)
  (string->zenkaku (string-downcase string)))


(defun string-zenkaku-upcase (string)
  (string->zenkaku (string-upcase string)))


(defun string-zenkaku-capitalize (string)
  (string->zenkaku (string-capitalize string)))


(defun kgm-process-running-p (proc)
  (and (processp proc)
       (eq (process-status proc) 'run)))


(defun erase-buffer-contents (buffer)
  (set-buffer buffer)
  (erase-buffer))


(defun copy-buffer-contents (dest source)
  (let ((lines))
    (set-buffer source)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ".+$" nil t)
	(if (match-beginning 0)
	    (push (buffer-substring (match-beginning 0) (match-end 0))
		  lines))))
    (set-buffer dest)
    (save-excursion
      (dolist (line (nreverse lines))
	(insert line) (insert "\n")))))


(defun set-buffer-read-only (buf)
  (set-buffer buf)
  (setq buffer-read-only t))


(defun inhibit-buffer-read-only (buf)
  (set-buffer buf)
  (setq buffer-read-only nil))


(defmacro with-buffer-read-only (buf &rest body)
  `(progn (inhibit-buffer-read-only ,buf)
	  ,@body
	  (set-buffer-read-only ,buf)))


(provide 'kgm-util)