;;;; kagami.el -- KAGAMI Kana Kanji Converter -*- coding: euc-jp; -*-

;; Author: Hirokuni Maeta<koji@kmc.gr.jp>
;; Keywords: japanese, kagami

;; This file is part of KAGAMI

;; Commentary:
;;
;; かな漢字変換エンジンKAGAMIをemacsから使うためのプログラム
;; leimとして使う。自前のquailパッケージを用意している。

(require 'cl)
(require 'hi-lock)

(require 'kgm-util)
(require 'kgm-data)
(require 'kgm-log)

; グローバル変数。
; このファイル内でしか使われない。

(defvar kgm-input-method-title "鏡"
  "String denoting KAGAMI input method is working, which is shown on mode line.")

(defvar kgm-agent-command-list '("~/kagami/bin/server")
  "kgm-agent の PATH")

(defvar kgm-agent-data-dir "~/kagami/model"
  "kgm-agent のデータがあるところ。")

(defvar kgm-working-buffer " *kgm*")

(defvar kgm-agent-process nil
  "kgm-agentのプロセス。")

(defvar kgm-accept-timeout 50)

(defvar kgm-server-log-file "~/kagami/log/server-log")

(defvar kgm-log-file "~/kagami/log/conversion-log"
  "変換を記録するためのファイル")

(defvar kgm-log-buffer " *kgmlog*")

(defvar kgm-log-list nil)

(defvar kgm-overlay-list nil
  "使用するoverlayはこれで一括で管理する。")

(defvar kgm-candidate-selector-chars "asdfghjkl;"
  "候補を選ぶときの文字。")

(defvar kgm-preedit-begin-mark "|")

(defvar kgm-preedit-delim-mark "|")

(defvar kgm-conversion-data nil
  "出力のためにいろいろな情報を集めるための変数。")

(defvar kgm-first-conversion-data nil
  "最初の変換で得られたデータ。")

(defvar kgm-origin-string-alist '((IV . "S") (TK . "T") (EX . "E")))

(defvar kgm-origin-overlay-alist '((IV . hi-yellow) (TK . hi-blue) (EX . hi-red)))

(defvar kgm-candidates-output-size (length kgm-candidate-selector-chars)
  "いくつの候補をミニバッファに表示するか")

(defvar kgm-show-conversion-list-count 2
  "kgm-next-prev-counterがこの数字以上になると、ミニバッファに候補のリストが表示される。")


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


;;; kgm-agentに関連する関数


;; シェルを指定してコマンドをスタート。
(defun kgm-start-shell-command (shell name buffer cmd &rest command-args)
  (setq shell-file-name shell)
  (apply 'start-process-shell-command name buffer cmd command-args))


(defun kgm-process-sentinel (proc stat)
  (message "%s" stat))


;; agentが走ってなかったら起動。
(defun kgm-check-agent ()
  (unless (kgm-process-running-p kgm-agent-process)
    (setq kgm-agent-process (kgm-invoke-agent))
    (set-process-sentinel kgm-agent-process 'kgm-process-sentinel)))


(defun kgm-kill-agent ()
  (when (kgm-process-running-p kgm-agent-process)
    (kill-process kgm-agent-process)))


;; agentを起動し、プロセスを返す。
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
          (buffer-string))              ; 文字列として返す
      (set-buffer old-buffer))))



;;; agentと通信する関数。

;;; agentの各コマンドを呼び、結果を返すだけ。
;;; ここで返ってくるデータは少しやばいので、いろいろと修正が必要。
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



;;; kgm-cmd-*系のラップ関数。

;;; 例えば、kmg-cmd-*系の関数は、
;;; 平仮名列を仮名漢字交じり列にするとき、シンボルを返したりするので
;;; ちゃんと文字列に直す。

;;; これでもまだLispでは扱いにくいので、下にある
;;; kgm-modify-convert-outputでもっと扱いやすくする。
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



;;; 上にあるconvert系の関数の出力を修正する。

;;; agentの返す生の構造を、candidate、 phrase、 conversionといった
;;; Lispが扱いやすい構造に直す。
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


;;; 出力がまともな関数。

;;; 実際に平仮名列を変換したり、候補を取ってくるのはこっちの関数にやらせる。

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



;; kanaの変換候補をとってきて、candと交ぜた配列をつくる。
(defun kgm-adjoin-cands (cand kana)
  (let ((other-cands (kgm-get-candidates kana))
	(conv-string (kgm-cand-string cand)))
    (let ((cands (cons cand
		       (remove-if ;候補に重複があれば、取り除く
			 #'(lambda (cand)
			     (string= conv-string
				      (kgm-cand-string cand)))
			 other-cands))))
      (apply #'vector cands))))



;;; オーバーレイに関する関数。

(defun kgm-delete-all-overlays ()
  (dolist (overlay kgm-overlay-list)
    (delete-overlay overlay))
  (setq kgm-overlay-list nil))


;; overlayをつくって設定した後、プットしてkgm-overlay-listにプッシュする。
(defun kgm-put-and-register-overlay (begin end prop value)
  (let ((overlay (make-overlay begin end)))
    (overlay-put overlay prop value)
    (push overlay kgm-overlay-list)))


;;; 現在のフレーズの変換候補をエコーエリアに表示できるようにするための関数。

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


;; selectorをミニバッファに表示させるために文字列にする。
(defun kgm-selector->string (cur-cand-idx selector)
  (let ((msgs (kgm-collect-cand-msgs cur-cand-idx selector)))
    (apply 'concat msgs)))


;; フレーズに従って、エコーエリアに表示させる候補文字列を構成する関数。
(defun kgm-make-minibuf-string (phrase)
  (kgm-selector->string
    (kgm-phr-cur-cand-idx phrase)
    (kgm-get-selector (kgm-phr-cur-cand-idx phrase)
		      (kgm-phr-selectors phrase))))


;; エコーエリアに現在の変換候補を表示。
(defun kgm-show-conversion-list (cur-phrase)
  (when (kgm-phr-selectors cur-phrase)  ;selectorがあるかを確認
    (let ((message-log-max nil))        ;*Message*バッファにログを残さない
      (message (kgm-make-minibuf-string cur-phrase)))))


;;; キーが押されたら呼ばれるコマンド

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


;; フレーズを新たに変換し直して、短くしたり長くしたりする関数。
(defun kgm-reconvert-phrases (cur-phr-idx phrases type)
  (let ((cur-phrase (aref phrases cur-phr-idx)))
    (let ((len   (length (kgm-phr-string cur-phrase))) ;現在の候補の平仮名の長さ
	  (input (loop for phr across (subseq phrases cur-phr-idx) ;現在の候補から残りの文字列全部
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
	:logP         0 ;logPはもういらない
	:phrases      new-phrases
	:phrases-size (length new-phrases)
	:cur-phr-idx  cur-phr-idx))))


;; フレーズを短くできるか？
(defun kgm-can-be-shorter-p (conversion)
  (kgm-with-current-phrase cur-phrase conversion
    (let ((cur-phr-string-len (length (kgm-phr-string cur-phrase))))
      (< 1 cur-phr-string-len))))


;; フレーズを長くできるか？
(defun kgm-can-be-longer-p (conversion)
  (let ((phrases-size (kgm-conv-phrases-size conversion))
	(cur-phr-idx  (kgm-conv-cur-phr-idx  conversion)))
    (< (1+ cur-phr-idx) phrases-size)))


;; フレーズを短くする。
;; 短くするフレーズが一番最初のものだったら、
;; kgm-conversion-dataにkgm-modified-convert-with-1st-boundaryの返り値をつっこむ
;; ように最初はしてたけど、コードが面倒くさくなってやめる(9/18)
(defun kgm-shorter ()
  (interactive)
  (when (kgm-can-be-shorter-p kgm-conversion-data)
    (setq kgm-conversion-data
	  (kgm-reconvert-conversion kgm-conversion-data 'shorter))))


;; フレーズを長くする。
;; 長くするフレーズが一番最初のものだったら、
;; kgm-conversion-dataにkgm-modified-convert-with-1st-boundaryの返り値をつっこむ
;; ように最初はしてたけど、コードが面倒くさくなってやめる(9/18)
(defun kgm-longer ()
  (interactive)
  (when (kgm-can-be-longer-p kgm-conversion-data)
    (setq kgm-conversion-data
	  (kgm-reconvert-conversion kgm-conversion-data 'longer))))



(defun kgm-to-hiragana ()
  (interactive)
  (kgm-with-current-phrase phr kgm-conversion-data
    ; -1 はひらがな
    (setf (kgm-phr-cur-cand-idx phr) -1)))


;; 要らないかなぁ
(defun kgm-to-katakana ()
  (interactive)
  (kgm-with-current-phrase phr kgm-conversion-data
    ; -2 はカタカナ
    (setf (kgm-phr-cur-cand-idx phr) -2)))



;; a,s,dなどが押されたときに呼ばれる。
;; 現在のフレーズの候補のインデックスを選ばれたものにする。
;; まちがって範囲外のものが押された可能性もあるので、そこら辺も考慮する。
(defun kgm-select-from-list ()
  (interactive)
  (let* ((selector-char last-input-event)
	 (index (position selector-char kgm-candidate-selector-chars)))
    (kgm-with-current-phrase cur-phrase kgm-conversion-data
      (when (kgm-phr-selectors cur-phrase)  ;まず、selectorがあるかを確認
	(let ((region
	       (kgm-selector-region
		 (kgm-get-selector (kgm-phr-cur-cand-idx cur-phrase)
				   (kgm-phr-selectors cur-phrase)))))
	  (let* ((from (car region))
		 (to   (cdr region))
		 (new-cand-idx (+ from index)))
	    (when (between new-cand-idx from to) ;はみ出してないか
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
  ;-1は全部ひらがなの状態
  (setf (kgm-conv-cur-phr-idx kgm-conversion-data) -1))


(defun kgm-accept ()
  (interactive)
  (setq kgm-converting nil))


;; 候補を増やした新しいphraseを返す。
(defun kgm-more-cands-phrase (phrase)
  (let ((cand (kgm-get-cur-cand phrase))
	(phr-str (kgm-phr-string phrase)))
    (let ((cands (kgm-adjoin-cands cand phr-str)))
      (make-kgm-phrase
        :string       phr-str
	:cur-cand-idx (kgm-phr-cur-cand-idx phrase)
	:cands-size   (length cands)
	:more-cands-p nil ;今候補を増やしたので、これ以上増やせない
	:cands        cands
	:selectors    (kgm-make-cand-selectors
		        (vector-to-list cands)
			kgm-candidates-output-size)
	:np-count     0))))


;; コマンドを実行する。
(defun kgm-do-command (cmd)
  (when (not (eq cmd 'kgm-accept))
    ;; cur-phr-idx = -1で確定もあり得るので
    ;; kgm-acceptはここと別にしないといけない
    (let ((index (kgm-conv-cur-phr-idx kgm-conversion-data))
	  (phrases (kgm-conv-phrases kgm-conversion-data)))
      (when (< index 0)
	(setf (kgm-conv-cur-phr-idx kgm-conversion-data) 0)
	(setf index 0))
      (when (or (eq cmd 'kgm-next-candidate)
		(eq cmd 'kgm-prev-candidate))
	;変換候補を増やさないといけない
	(let ((phrase (aref phrases index)))
	  (when (kgm-phr-more-cands-p phrase)
	    (setf (aref phrases index)
		  (kgm-more-cands-phrase phrase)))))))
  (call-interactively cmd))



;; 変換後にすること。
;; ログをファイルに書き込んだり。
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
	  ;普通の変換候補か
	  (cur-cand
   	   ;候補のoriginによってオーバーレイをつける。
	   (let* ((origin (kgm-cand-origin cur-cand))
		  (overlay (cdr (assoc origin kgm-origin-overlay-alist))))
	     (when overlay
	       (kgm-put-and-register-overlay from (point) 'face overlay))))	
  (t ;kwicするような分節でなかったとき
	     ;ここに、kwicのwindowを閉じるような操作を書きたい
	   ))))



;; フレーズを表示させる間数。
;; 変換の終わりのポインタ値を返す。
(defun kgm-display-phrases (begin end delim-p)
  (goto-char begin)
  (delete-region begin end)
  (kgm-delete-all-overlays)
  (let ((cur-phr-idx (kgm-conv-cur-phr-idx kgm-conversion-data)))
    (cond ((= cur-phr-idx -1) ;ひらがなの場合
	   (insert (kgm-conv-string kgm-conversion-data))
	   (when delim-p       ;アンダーラインだけ表示
	     (kgm-put-and-register-overlay begin (point) 'face 'underline)))
	  (t
	   ;通常の場合
	   (loop
	    with delim      = (if delim-p kgm-preedit-delim-mark "")
	    with begin-mark = (if delim-p kgm-preedit-begin-mark "")
	    with phrases    = (kgm-conv-phrases kgm-conversion-data)
	    for phrase across phrases
	    for i from 0
	    for from      = (1+ (point))   ;begin-mark, delimのハイライトはしない
	    for separator = (if (= i 0) begin-mark delim)
	    for focusedp  = (= i cur-phr-idx)
	    do      (kgm-insert-phrase phrase from separator focusedp)
	    finally (kgm-with-current-phrase cur-phr kgm-conversion-data
		        (kgm-put-and-register-overlay begin (point) 'face 'underline))))))
    (point))


;;; kgm-region

;; ローマ字入力してスペースを押すとこいつが呼ばれる。
;; 変換し終わった後、結局何文字になったのかを返さないと
;; 呼び出し元から怒られる。

;; 基本的に、
;; １、キーが押される。
;; ２、キーに対応する関数が呼ばれる。
;; ３、関数が、kgm-conversion-dataの値を変える。
;; ４、kgm-conversion-dataの値に従って、kgm-display-phrasesがフレーズを表示する。
;; の、ループ。
;; 変換が確定した後は、オーバーレイを全部消して、ログを取って、おわり。
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
	    ; これをnilにしないとinput-method-functionが
	    ; quail-input-methodのままでまずい。
	    (input-method-function nil))
	(while kgm-converting
	  (let* ((overriding-terminal-local-map kgm-keymap)
		 (keyseq (read-key-sequence nil))
		 (cmd (lookup-key kgm-keymap keyseq)))
	    (unless (commandp cmd)
              ; 変なキーが来た場合
	      ; 現在の変換をアクセプトして、変なキーを次の入力文字にする。
	      (setq unread-input-method-events
		    (append (string-to-list (this-single-command-raw-keys))
			    unread-input-method-events))
	      (setq cmd 'kgm-accept))
	    (kgm-do-command cmd) ;コマンド実行
	    (if (eq cmd 'kgm-accept)
		(setq end (kgm-display-phrases begin end nil))
	        (setq end (kgm-display-phrases begin end t)))))
	(- end begin)) ;これを返さないといけない
      (kgm-delete-all-overlays)
      (kgm-final-process))))




;;; 残り。

;; アドバイスを使っていろいろ設定する。
(defadvice toggle-input-method (before kgm-init-processes)
  "KAGAMIが使うプロセスを起動。"
  (let ((lang (get-language-info "Japanese" 'input-method)))
    (when (equal lang "japanese-kagami")
      (kgm-check-agent))))

(defadvice save-buffers-kill-emacs (before kgm-kill-all-processes)
  "KAGAMIが使うすべてのプロセスを殺し、ログがあれば書き込む。"
  (kgm-kill-agent)
  (kgm-write-log kgm-log-buffer (nreverse kgm-log-list) kgm-log-file))


(ad-activate 'toggle-input-method)
(ad-activate 'save-buffers-kill-emacs)


(provide 'kagami)
