;;;; kgm-data.el -- SLM Kana Kanji Converter -*- coding: euc-jp; -*-

;; Author: Hirokuni Maeta<koji@kmc.gr.jp>
;; Keywords: japanese

;; This file is part of KAGAMI

;;; Commentary:
;;
;; データ構造に関する関数



(require 'kgm-util)

;CONVERT
;平仮名列を仮名漢字交じり列に変換する。
; 例: (12.34 (脳 のう IN)(が が IN)(関与 かんよ IN))
;このときに区切られた各々の単語を分節とよぶことにする。
;ユーザーが平仮名列を入力して最初に表示されるのはこれ。

;LIST_CANDIDATE
;単語の変換候補を返す。
;最初の変換候補で正しいものがない場合
;スペースキーなどを押すとさらに候補が表示されるが、
;それはこれによって得られた候補。


;以下、データ構造の簡単な説明。

;変換候補の一つ分。
(defstruct kgm-candidate string origin logP)
(defalias 'kgm-cand-string 'kgm-candidate-string)
(defalias 'kgm-cand-origin 'kgm-candidate-origin)
(defalias 'kgm-cand-logP   'kgm-candidate-logP)


;agentに平仮名列を送ると、文節に区切られた仮名漢字交じり列が返される。
;phraseはその分節一つ分の情報。

;各文節に対して変換候補は複数ありうるので、
;それは、kgm-candidateの配列としてcandsで持っておく。
;cur-cand-idxはcandsのインデックスになっていて、
;該当するものは画面に表示される。


;more-cands-pは、さらに変換候補を取ってこれるかどうかを表す。
;CONVERTの結果をもとにphraseが作られるが、
;最初、more-cands-pは真になっている。
;LIST_CANDIDATEで候補を受け取ると、more-cands-pは偽になる。
;これ以上候補は増やせないということ。
;CONVERTがよばれたついでにLIST_CANDIDATEをするという手もあるが、
;それをやると遅くなる。
;それに、大抵はCONVERTで済む（だろう）

(defstruct kgm-phrase
  string
  cur-cand-idx
  cands-size
  more-cands-p
  cands
  selectors
  np-count      ;next, prevの総数
)
(defalias 'kgm-phr-string       'kgm-phrase-string)
(defalias 'kgm-phr-cur-cand-idx 'kgm-phrase-cur-cand-idx)
(defalias 'kgm-phr-cands-size   'kgm-phrase-cands-size)
(defalias 'kgm-phr-more-cands-p 'kgm-phrase-more-cands-p)
(defalias 'kgm-phr-cands        'kgm-phrase-cands)
(defalias 'kgm-phr-selectors    'kgm-phrase-selectors)
(defalias 'kgm-phr-np-count     'kgm-phrase-np-count)

;変換に関わるすべての変数を保持する構造である。

;日本語列は文節に区切られて、phraseという形になっている。
;全phraseは配列として、phrasesがもっている。
;cur-phr-idxが編集しているphrasesのインデックス。
;つまり、cur-phr-idxで示されるphraseで
;次や前の候補が得られたり、ハイライトが表示されている。
;-1のときは、全てひらがな表示。バックスぺースキーが押されたときなど。

;stringは、変換する平仮名列である。
;phrases-sizeは、phrasesの長さである。

(defstruct kgm-conversion
  string logP phrases phrases-size cur-phr-idx)
(defalias 'kgm-conv-string       'kgm-conversion-string)
(defalias 'kgm-conv-logP         'kgm-conversion-logP)
(defalias 'kgm-conv-phrases      'kgm-conversion-phrases)
(defalias 'kgm-conv-phrases-size 'kgm-conversion-phrases-size)
(defalias 'kgm-conv-cur-phr-idx  'kgm-conversion-cur-phr-idx)


; ちょっとしたマクロ。
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

;selectorは、ミニバッファに候補を見せるための構造。
;まず、全候補をリストにして、何個かに分ける。
;何個で分けるかは、kgm-candidates-output-sizeとフレーム幅で決まる。
;そして、等分されたリストに、
;最初の候補のインデックスと最後の候補のインデックスのコンスを付け加える。
;これでselectorをつくる。

(defstruct kgm-selector region cands)

(defun kgm-calc-width (candidate)
  (let ((selector-char-width 1)
	(string-width (string-width (kgm-cand-string candidate)))
	(origin-width (string-width (kgm-make-origin-string
				     (kgm-cand-origin candidate)))))
    (+ 1  selector-char-width 1 1 1 string-width 1  origin-width 1)))
    ;` a: [漢字](IV) 'を参考に幅を計算


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




;selectorのリストの中から、
;indexを挟むselectorを取り出す。
(defun kgm-get-selector (index selectors)
  (find-if #'(lambda (selector)
	       (let ((region (kgm-selector-region selector)))
		 (let ((from (car region))
		       (to   (cdr region)))
		   (between index from to))))
	   selectors))


;平仮名列がどのような仮名漢字交じり列に変換されたかという
;ログをとるためのデータ構造。

;timesは、変換されたときの時間。
;first-conv-formは、最初の変換候補。
;fixed-conv-formは、ユーザーが決定した変換。
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
	(kgm-cand-string cand)	       ;通常の場合
        (case (kgm-phr-cur-cand-idx phrase)
	  (-1 (kgm-phr-string phrase)) ;ひらがなの場合
	  (-2 (hiragana->katakana      ;カタカナの場合
	        (kgm-phr-string phrase)))))))

(provide 'kgm-data)
