;;;; kagami.el -- KAGAMI Kana Kanji Converter -*- coding: euc-jp; -*-

;; Author: Hirokuni Maeta<koji@kmc.gr.jp>
;; Keywords: japanese, kagami

;; This file is part of KAGAMI

;; Commentary:
;;
;; ���ʴ����Ѵ����󥸥�KAGAMI��emacs����Ȥ�����Υץ����
;; leim�Ȥ��ƻȤ���������quail�ѥå��������Ѱդ��Ƥ��롣

(require 'cl)
(require 'hi-lock)

(require 'kgm-util)
(require 'kgm-data)
(require 'kgm-log)

; �����Х��ѿ���
; ���Υե�������Ǥ����Ȥ��ʤ���

(defvar kgm-input-method-title "��"
  "String denoting KAGAMI input method is working, which is shown on mode line.")

(defvar kgm-agent-command-list '("~/kagami/bin/server")
  "kgm-agent �� PATH")

(defvar kgm-agent-data-dir "~/kagami/model"
  "kgm-agent �Υǡ���������Ȥ���")

(defvar kgm-working-buffer " *kgm*")

(defvar kgm-agent-process nil
  "kgm-agent�Υץ�����")

(defvar kgm-accept-timeout 50)

(defvar kgm-server-log-file "~/kagami/log/server-log")

(defvar kgm-log-file "~/kagami/log/conversion-log"
  "�Ѵ���Ͽ���뤿��Υե�����")

(defvar kgm-log-buffer " *kgmlog*")

(defvar kgm-log-list nil)

(defvar kgm-overlay-list nil
  "���Ѥ���overlay�Ϥ���ǰ��Ǵ������롣")

(defvar kgm-candidate-selector-chars "asdfghjkl;"
  "��������֤Ȥ���ʸ����")

(defvar kgm-preedit-begin-mark "|")

(defvar kgm-preedit-delim-mark "|")

(defvar kgm-conversion-data nil
  "���ϤΤ���ˤ�����ʾ���򽸤�뤿����ѿ���")

(defvar kgm-first-conversion-data nil
  "�ǽ���Ѵ�������줿�ǡ�����")

(defvar kgm-origin-string-alist '((IV . "S") (TK . "T") (EX . "E")))

(defvar kgm-origin-overlay-alist '((IV . hi-yellow) (TK . hi-blue) (EX . hi-red)))

(defvar kgm-candidates-output-size (length kgm-candidate-selector-chars)
  "�����Ĥθ����ߥ˥Хåե���ɽ�����뤫")

(defvar kgm-show-conversion-list-count 2
  "kgm-next-prev-counter�����ο����ʾ�ˤʤ�ȡ��ߥ˥Хåե��˸���Υꥹ�Ȥ�ɽ������롣")


(defvar kgm-keymap
  (let ((map (make-sparse-keymap))
	(len (length kgm-candidate-selector-chars))
	(i 0))
    (while (< i len)
      (define-key map
	(char-to-string
	  (aref kgm-candidate-selector-chars i))
	'kgm-select-from-list)
      (setq i (1+ i)))
    (define-key map " " 'kgm-next-candidate)
    (define-key map "\C-n" 'kgm-next-candidate)
    (define-key map "\C-p" 'kgm-prev-candidate)
    (define-key map "\C-f" 'kgm-next-phrase)
    (define-key map "\C-b" 'kgm-prev-phrase)
    (define-key map "\C-i" 'kgm-shorter)
    (define-key map "\C-o" 'kgm-longer)
    (define-key map "\C-a" 'kgm-move-beginning-of-phrases)
    (define-key map "\C-e" 'kgm-move-end-of-phrases)
    (define-key map "H"    'kgm-to-hiragana)
    (define-key map "K"    'kgm-to-katakana)
    (define-key map "\C-h" 'kgm-to-hiragana)
    (define-key map "\C-k" 'kgm-to-katakana)
    (define-key map "\C-w" 'kgm-move-top-of-candidates)
    (define-key map "\C-[" 'kgm-move-bottom-of-candidates)
    (define-key map "" 'kgm-cancel)
    (define-key map "" 'kgm-accept)
    map))


;;; kgm-agent�˴�Ϣ����ؿ�


;; ���������ꤷ�ƥ��ޥ�ɤ򥹥����ȡ�
(defun kgm-start-shell-command (shell name buffer cmd &rest command-args)
  (setq shell-file-name shell)
  (apply 'start-process-shell-command name buffer cmd command-args))


(defun kgm-process-sentinel (proc stat)
  (message "%s" stat))


;; agent�����äƤʤ��ä��鵯ư��
(defun kgm-check-agent ()
  (unless (kgm-process-running-p kgm-agent-process)
    (setq kgm-agent-process (kgm-invoke-agent))
    (set-process-sentinel kgm-agent-process 'kgm-process-sentinel)))


(defun kgm-kill-agent ()
  (when (kgm-process-running-p kgm-agent-process)
    (kill-process kgm-agent-process)))


;; agent��ư�����ץ������֤���
(defun kgm-invoke-agent ()
  (loop for cmd in kgm-agent-command-list
	for proc = (kgm-start-shell-command
		     "/bin/sh" "kgm-agent" kgm-working-buffer
		     cmd  kgm-agent-data-dir "2>" kgm-server-log-file)
	when (kgm-process-running-p proc)
	  return proc))


(defun kgm-send-recv-command (command)
  (kgm-check-agent)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer kgm-working-buffer)
          (erase-buffer)
          (process-send-string kgm-agent-process command) ; koko
          (while (= (buffer-size) 0)
            (accept-process-output nil 0 kgm-accept-timeout))
          (buffer-string))              ; ʸ����Ȥ����֤�
      (set-buffer old-buffer))))



;;; agent���̿�����ؿ���

;;; agent�γƥ��ޥ�ɤ�Ƥӡ���̤��֤�������
;;; �������֤äƤ���ǡ����Ͼ�����Ф��Τǡ�������Ƚ�����ɬ�ס�
(defun kgm-cmd-convert (input-string)
  (read
    (kgm-send-recv-command
      (concat "CONVERT " input-string "\n"))))


(defun kgm-cmd-convert-with-1st-boundary (input-string 1st-boundary)
  (read
    (kgm-send-recv-command
      (concat "CONVERT_WITH_1ST_BOUNDARY "
	      (number-to-string 1st-boundary) " "
	      input-string "\n"))))

(defun kgm-cmd-list-candidate (input-string)
  (read
    (kgm-send-recv-command
      (concat "LIST_CANDIDATE " input-string "\n"))))


(defun kgm-cmd-end ()
  (kgm-send-recv-command "END\n"))



;;; kgm-cmd-*�ϤΥ�å״ؿ���

;;; �㤨�С�kmg-cmd-*�Ϥδؿ��ϡ�
;;; ʿ��̾���̾�����򤸤���ˤ���Ȥ�������ܥ���֤����ꤹ��Τ�
;;; ������ʸ�����ľ����

;;; ����Ǥ�ޤ�Lisp�Ǥϰ����ˤ����Τǡ����ˤ���
;;; kgm-modify-convert-output�Ǥ�äȰ����䤹�����롣
(defun kgm-convert (input)
  (let ((result (kgm-cmd-convert input)))
    (cons (car result) ;logp
	  (mapcar #'(lambda (lst)
		      (destructuring-bind (cnv kana org) lst
			(list (symbol-name cnv) (symbol-name kana) org)))
		  (cdr result)))))


(defun kgm-convert-with-1st-boundary (input 1st-boundary)
  (let ((result (kgm-cmd-convert-with-1st-boundary input 1st-boundary)))
    (cons (car result) ;logp
	  (mapcar #'(lambda (lst)
		      (destructuring-bind (cnv kana org) lst
			(list (symbol-name cnv) (symbol-name kana) org)))
		  (cdr result)))))



;;; ��ˤ���convert�Ϥδؿ��ν��Ϥ������롣

;;; agent���֤����ι�¤��candidate�� phrase�� conversion�Ȥ��ä�
;;; Lisp�������䤹����¤��ľ����
(defun kgm-modify-convert-output (command-output input)
  (flet ((mk-phrase (lst)
	   (destructuring-bind (string kana org) lst
	     (let ((cand (make-kgm-candidate
			   :string string :origin org :logP 0)))
	       (make-kgm-phrase :string kana  :cur-cand-idx 0
				:cands-size 1 :more-cands-p t
			        :cands (vector cand))))))
    (let ((logP (car command-output))
	  (conv (cdr command-output)))
      (let ((phrase-list (mapcar #'mk-phrase conv)))
	(make-kgm-conversion :string input :logP logP
			     :phrases      (apply #'vector phrase-list)
	                     :phrases-size (length phrase-list)
	                     :cur-phr-idx 0)))))


;;; ���Ϥ��ޤȤ�ʴؿ���

;;; �ºݤ�ʿ��̾����Ѵ������ꡢ������äƤ���ΤϤ��ä��δؿ��ˤ�餻�롣

(defun kgm-modified-convert-with-1st-boundary (input 1st-boundary)
  (kgm-modify-convert-output
    (kgm-convert-with-1st-boundary input 1st-boundary) input))


(defun kgm-modified-convert (input)
  (kgm-modify-convert-output (kgm-convert input) input))


(defun kgm-get-candidates (input)
  (let ((candidates (kgm-cmd-list-candidate input)))
    (mapcar #'(lambda (candidate)
		(destructuring-bind (logP conv org) candidate
		  (make-kgm-candidate
		    :string (symbol-name conv) :origin org :logP logP)))
	    candidates)))



;; kana���Ѵ������ȤäƤ��ơ�cand�ȸ򤼤������Ĥ��롣
(defun kgm-adjoin-cands (cand kana)
  (let ((other-cands (kgm-get-candidates kana))
	(conv-string (kgm-cand-string cand)))
    (let ((cands (cons cand
		       (remove-if ;����˽�ʣ������С�������
			 #'(lambda (cand)
			     (string= conv-string
				      (kgm-cand-string cand)))
			 other-cands))))
      (apply #'vector cands))))



;;; �����С��쥤�˴ؤ���ؿ���

(defun kgm-delete-all-overlays ()
  (dolist (overlay kgm-overlay-list)
    (delete-overlay overlay))
  (setq kgm-overlay-list nil))


;; overlay��Ĥ��ä����ꤷ���塢�ץåȤ���kgm-overlay-list�˥ץå��夹�롣
(defun kgm-put-and-register-overlay (begin end prop value)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay prop value)
    (push overlay kgm-overlay-list)))


;;; ���ߤΥե졼�����Ѵ�����򥨥������ꥢ��ɽ���Ǥ���褦�ˤ��뤿��δؿ���

(defun kgm-make-origin-string (org)
  (let ((pair (assoc org kgm-origin-string-alist)))
    (if pair (concat " (" (cdr pair) ")") "")))


(defun kgm-get-cand-selector-char (index)
  (char-to-string (aref kgm-candidate-selector-chars index)))


(defun kgm-make-cand-msg (selector-char candidate-string origin-string focusedp)
  (let ((msg nil))
    (if focusedp
	(progn
	  (setq msg (format " %s: [%s]%s " selector-char candidate-string origin-string))
	  (put-text-property (position ?\[ msg)  (1+ (position ?\] msg)) 'face 'highlight msg))
        (setq msg (format " %s: %s%s "   selector-char candidate-string origin-string)))
    msg))


(defun kgm-collect-cand-msgs (cur-cand-idx selector)
  (let ((region (kgm-selector-region selector))
	(cands  (kgm-selector-cands  selector)))
    (let ((index (- cur-cand-idx (car region))))
      (loop for i from 0
	    for cand in cands
	    for focusedp    = (= i index)
	    for sel-char    = (kgm-get-cand-selector-char i)
	    for cand-string = (kgm-cand-string cand)
	    for org-string  = (kgm-make-origin-string (kgm-cand-origin cand))
	    collect (kgm-make-cand-msg sel-char cand-string org-string focusedp)))))


;; selector��ߥ˥Хåե���ɽ�������뤿���ʸ����ˤ��롣
(defun kgm-selector->string (cur-cand-idx selector)
  (let ((msgs (kgm-collect-cand-msgs cur-cand-idx selector)))
    (apply 'concat msgs)))


;; �ե졼���˽��äơ����������ꥢ��ɽ�����������ʸ�����������ؿ���
(defun kgm-make-minibuf-string (phrase)
  (kgm-selector->string
    (kgm-phr-cur-cand-idx phrase)
    (kgm-get-selector (kgm-phr-cur-cand-idx phrase)
		      (kgm-phr-selectors phrase))))


;; ���������ꥢ�˸��ߤ��Ѵ������ɽ����
(defun kgm-show-conversion-list (cur-phrase)
  (when (kgm-phr-selectors cur-phrase)  ;selector�����뤫���ǧ
    (let ((message-log-max nil))        ;*Message*�Хåե��˥���Ĥ��ʤ�
      (message (kgm-make-minibuf-string cur-phrase)))))


;;; �����������줿��ƤФ�륳�ޥ��

(defun kgm-move-top-of-candidates ()
  (interactive)
  (kgm-with-current-phrase cur-phrase kgm-conversion-data
    (setf (kgm-phr-cur-cand-idx cur-phrase) 0)
    (kgm-show-conversion-list cur-phrase)))


(defun kgm-move-bottom-of-candidates ()
  (interactive)
  (kgm-with-current-phrase cur-phrase kgm-conversion-data
    (setf (kgm-phr-cur-cand-idx cur-phrase)
	  (1- (kgm-phr-cands-size cur-phrase)))
    (kgm-show-conversion-list cur-phrase)))


(defun kgm-next-candidate ()
  (interactive)
  (kgm-with-current-phrase cur-phrase kgm-conversion-data
    (let ((index (kgm-phr-cur-cand-idx cur-phrase))
	  (size  (kgm-phr-cands-size     cur-phrase)))
      (if (or (= index (1- size)) (< index 0))
	  (setf (kgm-phr-cur-cand-idx cur-phrase) 0)
	  (setf (kgm-phr-cur-cand-idx cur-phrase) (1+ index)))
      (incf (kgm-phr-np-count cur-phrase))
      (when (<= kgm-show-conversion-list-count
		(kgm-phr-np-count cur-phrase))
	(kgm-show-conversion-list cur-phrase)))))


(defun kgm-prev-candidate ()
  (interactive)
  (kgm-with-current-phrase cur-phrase kgm-conversion-data
    (let ((index (kgm-phr-cur-cand-idx cur-phrase))
	  (size  (kgm-phr-cands-size   cur-phrase)))
      (if (<= index 0)
	  (setf (kgm-phr-cur-cand-idx cur-phrase) (1- size))
	  (setf (kgm-phr-cur-cand-idx cur-phrase) (1- index)))
      (incf (kgm-phr-np-count cur-phrase))
      (when (<= kgm-show-conversion-list-count
		(kgm-phr-np-count cur-phrase))
	(kgm-show-conversion-list cur-phrase)))))


(defun kgm-move-beginning-of-phrases ()
  (interactive)
  (setf (kgm-conv-cur-phr-idx kgm-conversion-data) 0))


(defun kgm-move-end-of-phrases ()
  (interactive)
  (setf (kgm-conv-cur-phr-idx kgm-conversion-data)
	(1- (kgm-conv-phrases-size kgm-conversion-data))))


;; �ե졼���򿷤����Ѵ���ľ���ơ�û��������Ĺ�������ꤹ��ؿ���
(defun kgm-reconvert-phrases (cur-phr-idx phrases type)
  (let ((cur-phrase (aref phrases cur-phr-idx)))
    (let ((len   (length (kgm-phr-string cur-phrase))) ;���ߤθ����ʿ��̾��Ĺ��
	  (input (loop for phr across (subseq phrases cur-phr-idx) ;���ߤθ��䤫��Ĥ��ʸ��������
		       concat (kgm-phr-string phr))))
      (let ((conv
	     (case type
	       (longer  (kgm-modified-convert-with-1st-boundary input (1+ len)))
	       (shorter (kgm-modified-convert-with-1st-boundary input (1- len))))))
	(let ((phs1 (subseq phrases 0 cur-phr-idx))
	      (phs2 (kgm-conv-phrases conv)))
	  (vconcat phs1 phs2))))))


(defun kgm-reconvert-conversion (conversion type)
  (let ((old-phrases (kgm-conv-phrases     conversion))
	(cur-phr-idx (kgm-conv-cur-phr-idx conversion)))
    (let ((new-phrases
	   (kgm-reconvert-phrases cur-phr-idx old-phrases type)))
      (make-kgm-conversion
        :string       (kgm-conv-string conversion)
	:logP         0 ;logP�Ϥ⤦����ʤ�
	:phrases      new-phrases
	:phrases-size (length new-phrases)
	:cur-phr-idx  cur-phr-idx))))


;; �ե졼����û���Ǥ��뤫��
(defun kgm-can-be-shorter-p (conversion)
  (kgm-with-current-phrase cur-phrase conversion
    (let ((cur-phr-string-len (length (kgm-phr-string cur-phrase))))
      (< 1 cur-phr-string-len))))


;; �ե졼����Ĺ���Ǥ��뤫��
(defun kgm-can-be-longer-p (conversion)
  (let ((phrases-size (kgm-conv-phrases-size conversion))
	(cur-phr-idx  (kgm-conv-cur-phr-idx  conversion)))
    (< (1+ cur-phr-idx) phrases-size)))


;; �ե졼����û�����롣
;; û������ե졼�������ֺǽ�Τ�Τ��ä��顢
;; kgm-conversion-data��kgm-modified-convert-with-1st-boundary���֤��ͤ�Ĥä���
;; �褦�˺ǽ�Ϥ��Ƥ����ɡ������ɤ����ݤ������ʤäƤ���(9/18)
(defun kgm-shorter ()
  (interactive)
  (when (kgm-can-be-shorter-p kgm-conversion-data)
    (setq kgm-conversion-data
	  (kgm-reconvert-conversion kgm-conversion-data 'shorter))))


;; �ե졼����Ĺ�����롣
;; Ĺ������ե졼�������ֺǽ�Τ�Τ��ä��顢
;; kgm-conversion-data��kgm-modified-convert-with-1st-boundary���֤��ͤ�Ĥä���
;; �褦�˺ǽ�Ϥ��Ƥ����ɡ������ɤ����ݤ������ʤäƤ���(9/18)
(defun kgm-longer ()
  (interactive)
  (when (kgm-can-be-longer-p kgm-conversion-data)
    (setq kgm-conversion-data
	  (kgm-reconvert-conversion kgm-conversion-data 'longer))))



(defun kgm-to-hiragana ()
  (interactive)
  (kgm-with-current-phrase phr kgm-conversion-data
    ; -1 �ϤҤ餬��
    (setf (kgm-phr-cur-cand-idx phr) -1)))


;; �פ�ʤ����ʤ�
(defun kgm-to-katakana ()
  (interactive)
  (kgm-with-current-phrase phr kgm-conversion-data
    ; -2 �ϥ�������
    (setf (kgm-phr-cur-cand-idx phr) -2)))



;; a,s,d�ʤɤ������줿�Ȥ��˸ƤФ�롣
;; ���ߤΥե졼���θ���Υ���ǥå��������Ф줿��Τˤ��롣
;; �ޤ����ä��ϰϳ��Τ�Τ������줿��ǽ���⤢��Τǡ��������դ��θ���롣
(defun kgm-select-from-list ()
  (interactive)
  (let* ((selector-char last-input-event)
	 (index (position selector-char kgm-candidate-selector-chars)))
    (kgm-with-current-phrase cur-phrase kgm-conversion-data
      (when (kgm-phr-selectors cur-phrase)  ;�ޤ���selector�����뤫���ǧ
	(let ((region
	       (kgm-selector-region
		 (kgm-get-selector (kgm-phr-cur-cand-idx cur-phrase)
				   (kgm-phr-selectors cur-phrase)))))
	  (let* ((from (car region))
		 (to   (cdr region))
		 (new-cand-idx (+ from index)))
	    (when (between new-cand-idx from to) ;�Ϥ߽Ф��Ƥʤ���
	      (setf (kgm-phr-cur-cand-idx cur-phrase) new-cand-idx))
	    (kgm-show-conversion-list cur-phrase)))))))



(defun kgm-next-phrase ()
  (interactive)
  (let ((index (kgm-conv-cur-phr-idx  kgm-conversion-data))
	(size  (kgm-conv-phrases-size kgm-conversion-data)))
    (when (< index (1- size))
      (incf (kgm-conv-cur-phr-idx kgm-conversion-data)))))


(defun kgm-prev-phrase ()
  (interactive)
  (let ((index (kgm-conv-cur-phr-idx kgm-conversion-data)))
    (when (< 0 index)
      (decf (kgm-conv-cur-phr-idx kgm-conversion-data)))))


(defun kgm-cancel ()
  (interactive)
  ;-1�������Ҥ餬�ʤξ���
  (setf (kgm-conv-cur-phr-idx kgm-conversion-data) -1))


(defun kgm-accept ()
  (interactive)
  (setq kgm-converting nil))


;; ��������䤷��������phrase���֤���
(defun kgm-more-cands-phrase (phrase)
  (let ((cand (kgm-get-cur-cand phrase))
	(phr-str (kgm-phr-string phrase)))
    (let ((cands (kgm-adjoin-cands cand phr-str)))
      (make-kgm-phrase
        :string       phr-str
	:cur-cand-idx (kgm-phr-cur-cand-idx phrase)
	:cands-size   (length cands)
	:more-cands-p nil ;����������䤷���Τǡ�����ʾ����䤻�ʤ�
	:cands        cands
	:selectors    (kgm-make-cand-selectors
		        (vector-to-list cands)
			kgm-candidates-output-size)
	:np-count     0))))


;; ���ޥ�ɤ�¹Ԥ��롣
(defun kgm-do-command (cmd)
  (when (not (eq cmd 'kgm-accept))
    ;; cur-phr-idx = -1�ǳ���⤢������Τ�
    ;; kgm-accept�Ϥ������̤ˤ��ʤ��Ȥ����ʤ�
    (let ((index (kgm-conv-cur-phr-idx kgm-conversion-data))
	  (phrases (kgm-conv-phrases kgm-conversion-data)))
      (when (< index 0)
	(setf (kgm-conv-cur-phr-idx kgm-conversion-data) 0)
	(setf index 0))
      (when (or (eq cmd 'kgm-next-candidate)
		(eq cmd 'kgm-prev-candidate))
	;�Ѵ���������䤵�ʤ��Ȥ����ʤ�
	(let ((phrase (aref phrases index)))
	  (when (kgm-phr-more-cands-p phrase)
	    (setf (aref phrases index)
		  (kgm-more-cands-phrase phrase)))))))
  (call-interactively cmd))



;; �Ѵ���ˤ��뤳�ȡ�
;; ����ե�����˽񤭹�����ꡣ
(defun kgm-final-process ()
  (let ((log (kgm-create-new-log
	       kgm-first-conversion-data
	       kgm-conversion-data)))
    (push log kgm-log-list)
    (when (= (length kgm-log-list) 10)
      (kgm-write-log
        kgm-log-buffer (nreverse kgm-log-list) kgm-log-file)
      (setq kgm-log-list nil))))


(defun kgm-insert-phrase (phrase from delim focusedp)
  (let ((cur-cand (kgm-get-cur-cand phrase))
	(phr-str  (kgm-get-phr-string phrase)))
    (insert (concat delim phr-str))
    (cond (focusedp
	   (kgm-put-and-register-overlay from (point) 'face 'highlight))
	  ;���̤��Ѵ����䤫
	  (cur-cand
   	   ;�����origin�ˤ�äƥ����С��쥤��Ĥ��롣
	   (let* ((origin (kgm-cand-origin cur-cand))
		  (overlay (cdr (assoc origin kgm-origin-overlay-alist))))
	     (when overlay
	       (kgm-put-and-register-overlay from (point) 'face overlay))))	
  (t ;kwic����褦��ʬ��Ǥʤ��ä��Ȥ�
	     ;�����ˡ�kwic��window���Ĥ���褦������񤭤���
	   ))))



;; �ե졼����ɽ��������ֿ���
;; �Ѵ��ν����Υݥ����ͤ��֤���
(defun kgm-display-phrases (begin end delim-p)
  (goto-char begin)
  (delete-region begin end)
  (kgm-delete-all-overlays)
  (let ((cur-phr-idx (kgm-conv-cur-phr-idx kgm-conversion-data)))
    (cond ((= cur-phr-idx -1) ;�Ҥ餬�ʤξ��
	   (insert (kgm-conv-string kgm-conversion-data))
	   (when delim-p       ;��������饤�����ɽ��
	     (kgm-put-and-register-overlay begin (point) 'face 'underline)))
	  (t
	   ;�̾�ξ��
	   (loop
	    with delim      = (if delim-p kgm-preedit-delim-mark "")
	    with begin-mark = (if delim-p kgm-preedit-begin-mark "")
	    with phrases    = (kgm-conv-phrases kgm-conversion-data)
	    for phrase across phrases
	    for i from 0
	    for from      = (1+ (point))   ;begin-mark, delim�Υϥ��饤�ȤϤ��ʤ�
	    for separator = (if (= i 0) begin-mark delim)
	    for focusedp  = (= i cur-phr-idx)
	    do      (kgm-insert-phrase phrase from separator focusedp)
	    finally (kgm-with-current-phrase cur-phr kgm-conversion-data
		        (kgm-put-and-register-overlay begin (point) 'face 'underline))))))
    (point))


;;; kgm-region

;; ���޻����Ϥ��ƥ��ڡ����򲡤��Ȥ����Ĥ��ƤФ�롣
;; �Ѵ�������ä��塢��ɲ�ʸ���ˤʤä��Τ����֤��ʤ���
;; �ƤӽФ��������ܤ��롣

;; ����Ū�ˡ�
;; ����������������롣
;; �����������б�����ؿ����ƤФ�롣
;; �����ؿ�����kgm-conversion-data���ͤ��Ѥ��롣
;; ����kgm-conversion-data���ͤ˽��äơ�kgm-display-phrases���ե졼����ɽ�����롣
;; �Ρ��롼�ס�
;; �Ѵ������ꤷ����ϡ������С��쥤�������ä��ơ������äơ�����ꡣ
(defun kgm-region (from to)
  (interactive "r")
  (let ((input (buffer-substring from to))
	(begin from) (end to))
    (setq kgm-first-conversion-data (kgm-modified-convert input))
    (setq kgm-conversion-data       (kgm-modified-convert input))
    (setq kgm-converting           t)
    (setq end (kgm-display-phrases begin end t))
    (unwind-protect
      (let ((current-input-method-title kgm-input-method-title)
	    ; �����nil�ˤ��ʤ���input-method-function��
	    ; quail-input-method�ΤޤޤǤޤ�����
	    (input-method-function nil))
	(while kgm-converting
	  (let* ((overriding-terminal-local-map kgm-keymap)
		 (keyseq (read-key-sequence nil))
		 (cmd (lookup-key kgm-keymap keyseq)))
	    (unless (commandp cmd)
              ; �Ѥʥ������褿���
	      ; ���ߤ��Ѵ��򥢥����ץȤ��ơ��Ѥʥ����򼡤�����ʸ���ˤ��롣
	      (setq unread-input-method-events
		    (append (string-to-list (this-single-command-raw-keys))
			    unread-input-method-events))
	      (setq cmd 'kgm-accept))
	    (kgm-do-command cmd) ;���ޥ�ɼ¹�
	    (if (eq cmd 'kgm-accept)
		(setq end (kgm-display-phrases begin end nil))
	        (setq end (kgm-display-phrases begin end t)))))
	(- end begin)) ;������֤��ʤ��Ȥ����ʤ�
      (kgm-delete-all-overlays)
      (kgm-final-process))))




;;; �Ĥꡣ

;; ���ɥХ�����ȤäƤ��������ꤹ�롣
(defadvice toggle-input-method (before kgm-init-processes)
  "KAGAMI���Ȥ��ץ�����ư��"
  (let ((lang (get-language-info "Japanese" 'input-method)))
    (when (equal lang "japanese-kagami")
      (kgm-check-agent))))

(defadvice save-buffers-kill-emacs (before kgm-kill-all-processes)
  "KAGAMI���Ȥ����٤ƤΥץ����򻦤�����������н񤭹��ࡣ"
  (kgm-kill-agent)
  (kgm-write-log kgm-log-buffer (nreverse kgm-log-list) kgm-log-file))


(ad-activate 'toggle-input-method)
(ad-activate 'save-buffers-kill-emacs)


(provide 'kagami)
