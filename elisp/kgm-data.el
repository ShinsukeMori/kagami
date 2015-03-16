;;;; kgm-data.el -- SLM Kana Kanji Converter -*- coding: euc-jp; -*-

;; Author: Hirokuni Maeta<koji@kmc.gr.jp>
;; Keywords: japanese

;; This file is part of KAGAMI

;;; Commentary:
;;
;; �ǡ�����¤�˴ؤ���ؿ�



(require 'kgm-util)

;CONVERT
;ʿ��̾���̾�����򤸤�����Ѵ����롣
; ��: (12.34 (Ǿ �Τ� IN)(�� �� IN)(��Ϳ ����� IN))
;���ΤȤ��˶��ڤ�줿�ơ���ñ���ʬ��Ȥ�֤��Ȥˤ��롣
;�桼������ʿ��̾������Ϥ��ƺǽ��ɽ�������ΤϤ��졣

;LIST_CANDIDATE
;ñ����Ѵ�������֤���
;�ǽ���Ѵ��������������Τ��ʤ����
;���ڡ��������ʤɤ򲡤��Ȥ���˸��䤬ɽ������뤬��
;����Ϥ���ˤ�ä�����줿���䡣


;�ʲ����ǡ�����¤�δ�ñ��������

;�Ѵ�����ΰ��ʬ��
(defstruct kgm-candidate string origin logP)
(defalias 'kgm-cand-string 'kgm-candidate-string)
(defalias 'kgm-cand-origin 'kgm-candidate-origin)
(defalias 'kgm-cand-logP   'kgm-candidate-logP)


;agent��ʿ��̾�������ȡ�ʸ��˶��ڤ�줿��̾�����򤸤����֤���롣
;phrase�Ϥ���ʬ����ʬ�ξ���

;��ʸ����Ф����Ѵ������ʣ�����ꤦ��Τǡ�
;����ϡ�kgm-candidate������Ȥ���cands�ǻ��äƤ�����
;cur-cand-idx��cands�Υ���ǥå����ˤʤäƤ��ơ�
;���������Τϲ��̤�ɽ������롣


;more-cands-p�ϡ�������Ѵ�������äƤ���뤫�ɤ�����ɽ����
;CONVERT�η�̤��Ȥ�phrase������뤬��
;�ǽ顢more-cands-p�Ͽ��ˤʤäƤ��롣
;LIST_CANDIDATE�Ǹ����������ȡ�more-cands-p�ϵ��ˤʤ롣
;����ʾ��������䤻�ʤ��Ȥ������ȡ�
;CONVERT����Ф줿�Ĥ��Ǥ�LIST_CANDIDATE�򤹤�Ȥ�����⤢�뤬��
;���������٤��ʤ롣
;����ˡ������CONVERT�ǺѤ�ʤ�����

(defstruct kgm-phrase
  string
  cur-cand-idx
  cands-size
  more-cands-p
  cands
  selectors
  np-count      ;next, prev�����
)
(defalias 'kgm-phr-string       'kgm-phrase-string)
(defalias 'kgm-phr-cur-cand-idx 'kgm-phrase-cur-cand-idx)
(defalias 'kgm-phr-cands-size   'kgm-phrase-cands-size)
(defalias 'kgm-phr-more-cands-p 'kgm-phrase-more-cands-p)
(defalias 'kgm-phr-cands        'kgm-phrase-cands)
(defalias 'kgm-phr-selectors    'kgm-phrase-selectors)
(defalias 'kgm-phr-np-count     'kgm-phrase-np-count)

;�Ѵ��˴ؤ�뤹�٤Ƥ��ѿ����ݻ����빽¤�Ǥ��롣

;���ܸ����ʸ��˶��ڤ��ơ�phrase�Ȥ������ˤʤäƤ��롣
;��phrase������Ȥ��ơ�phrases����äƤ��롣
;cur-phr-idx���Խ����Ƥ���phrases�Υ���ǥå�����
;�Ĥޤꡢcur-phr-idx�Ǽ������phrase��
;�������θ��䤬����줿�ꡢ�ϥ��饤�Ȥ�ɽ������Ƥ��롣
;-1�ΤȤ��ϡ����ƤҤ餬��ɽ�����Хå����ڡ��������������줿�Ȥ��ʤɡ�

;string�ϡ��Ѵ�����ʿ��̾��Ǥ��롣
;phrases-size�ϡ�phrases��Ĺ���Ǥ��롣

(defstruct kgm-conversion
  string logP phrases phrases-size cur-phr-idx)
(defalias 'kgm-conv-string       'kgm-conversion-string)
(defalias 'kgm-conv-logP         'kgm-conversion-logP)
(defalias 'kgm-conv-phrases      'kgm-conversion-phrases)
(defalias 'kgm-conv-phrases-size 'kgm-conversion-phrases-size)
(defalias 'kgm-conv-cur-phr-idx  'kgm-conversion-cur-phr-idx)


; ����äȤ����ޥ���
(defmacro kgm-with-phrases (var conv &rest body)
  (let ((g (gensym)))
    `(let ((,g ,conv))
       (let ((,var (kgm-conv-phrases ,conv)))
	 ,@body))))


(defmacro kgm-with-current-phrase (var conv &rest body)
  (let ((g (gensym)))
    `(let ((,g ,conv))
       (let ((,var (aref (kgm-conv-phrases ,g)
			  (kgm-conv-cur-phr-idx ,g))))
	 ,@body))))

;selector�ϡ��ߥ˥Хåե��˸���򸫤��뤿��ι�¤��
;�ޤ����������ꥹ�Ȥˤ��ơ����Ĥ���ʬ���롣
;���Ĥ�ʬ���뤫�ϡ�kgm-candidates-output-size�ȥե졼�����Ƿ�ޤ롣
;�����ơ���ʬ���줿�ꥹ�Ȥˡ�
;�ǽ�θ���Υ���ǥå����ȺǸ�θ���Υ���ǥå����Υ��󥹤��դ��ä��롣
;�����selector��Ĥ��롣

(defstruct kgm-selector region cands)

(defun kgm-calc-width (candidate)
  (let ((selector-char-width 1)
	(string-width (string-width (kgm-cand-string candidate)))
	(origin-width (string-width (kgm-make-origin-string
				     (kgm-cand-origin candidate)))))
    (+ 1  selector-char-width 1 1 1 string-width 1  origin-width 1)))
    ;` a: [����](IV) '�򻲹ͤ�����׻�


(defun kgm-make-cand-selectors (candidates max-size)
  (let ((frame-width (frame-width))
	(total-width (kgm-calc-width (car candidates)))
	(total-size  1)
	(tmp (list (car candidates)))
	(from 0)
	(ret nil))
    (loop for candidate in (cdr candidates)
	  for index from 1
       do (let ((cur-width (kgm-calc-width candidate)))
	    (cond ((and (<= (1+ total-size) max-size)
			(< (+ cur-width total-width) frame-width))
		   (push candidate tmp)
		   (incf total-size)
		   (incf total-width cur-width))
		  (t
		   (let ((selector (make-kgm-selector
				     :region (cons from (+ from (1- total-size)))
				     :cands  (nreverse tmp))))
		     (push selector ret)
		     (setq tmp (list candidate))
		     (setq total-width cur-width)
		     (setq total-size  1)
		     (setq from index)))))
       finally (when tmp
		(let ((selector (make-kgm-selector
				  :region (cons from (+ from (1- total-size)))
				  :cands (nreverse tmp))))
		  (push selector ret))))
    (nreverse ret)))




;selector�Υꥹ�Ȥ��椫�顢
;index�򶴤�selector����Ф���
(defun kgm-get-selector (index selectors)
  (find-if #'(lambda (selector)
	       (let ((region (kgm-selector-region selector)))
		 (let ((from (car region))
		       (to   (cdr region)))
		   (between index from to))))
	   selectors))


;ʿ��̾�󤬤ɤΤ褦�ʲ�̾�����򤸤�����Ѵ����줿���Ȥ���
;����Ȥ뤿��Υǡ�����¤��

;times�ϡ��Ѵ����줿�Ȥ��λ��֡�
;first-conv-form�ϡ��ǽ���Ѵ����䡣
;fixed-conv-form�ϡ��桼���������ꤷ���Ѵ���
(defstruct kgm-log times first-conv-form fixed-conv-form)

(defun kgm-get-cur-cand (phrase)
  (let ((cands (kgm-phr-cands phrase))
	(index (kgm-phr-cur-cand-idx phrase)))
    (if (<= 0 index)
	(aref cands index)
        nil)))


(defun kgm-get-phr-string (phrase)
  (let ((cand (kgm-get-cur-cand phrase)))
    (if cand
	(kgm-cand-string cand)	       ;�̾�ξ��
        (case (kgm-phr-cur-cand-idx phrase)
	  (-1 (kgm-phr-string phrase)) ;�Ҥ餬�ʤξ��
	  (-2 (hiragana->katakana      ;�������ʤξ��
	        (kgm-phr-string phrase)))))))

(provide 'kgm-data)
