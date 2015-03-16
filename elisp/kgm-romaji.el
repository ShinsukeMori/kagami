
(require 'kgm-util)
(require 'kgm-data)


(defvar kgm-romaji-input-method-title "R")

(defvar kgm-romaji-conversion nil)

(defvar kgm-romaji-current-phrase-index 0)

(defvar kgm-romaji-converting nil)

(defvar kgm-romaji-keymap
  (let ((map (make-sparse-keymap))
	(len (length kgm-candidate-selector-chars))
	(i 0))
    (while (< i len)
      (let ((selector-char (char-to-string
			     (aref kgm-candidate-selector-chars i))))
	(define-key map selector-char 'kgm-romaji-select-from-list)
	(setq i (1+ i))))
    (define-key map " "    'kgm-romaji-next-candidate)
    (define-key map "\C-n" 'kgm-romaji-next-candidate)
    (define-key map "\C-p" 'kgm-romaji-prev-candidate)
    ;なぜか、こうしないとうまくいかない
    (define-key map "" 'kgm-romaji-accept)
    map))


(defun kgm-make-romaji-conversion (hankaku)
  (let* ((downcase   (string-downcase   hankaku))
	 (upcase     (string-upcase     hankaku))
	 (capitalize (string-capitalize hankaku))
	 (zenkaku    (string-to-zenkaku hankaku))
	 (zenkaku-downcase   (string-downcase   zenkaku))
	 (zenkaku-upcase     (string-upcase     zenkaku))
	 (zenkaku-capitalize (string-capitalize zenkaku))
	 (romajis
	  (list hankaku downcase upcase capitalize
		zenkaku-downcase zenkaku-upcase zenkaku-capitalize)))
    (let* ((cands
	    (mapcar (lambda (romaji)
		      (make-kgm-candidate :string romaji :origin nil :logP 0))
		    romajis))
	   (phrase
	    (make-kgm-phrase :string       hankaku
			     :cur-cand-idx 0
			     :cands-size   (length cands)
			     :more-cands-p nil
			     :cands        (apply #'vector cands)
			     :selectors    (kgm-make-cand-selectors cands 10)
			     :np-count     0)))
      (kgm-make-conversion
        :string       hankaku
	:logP         0
	:phrases      (vector phrase)
	:phrases-size 1))))
      

;;10/17

(defun kgm-romaji-next-candidate ()
  (interactive)
  (let ((cur-phrase
	 (aref (kgm-conversion-phrases kgm-romaji-conversion)
	       kgm-romaji-current-phrase-index)))
    (incf (kgm-phrase-next-prev-counter cur-phrase))
    (let ((index (kgm-phrase-cur-candidate-index cur-phrase))
	  (size  (kgm-phrase-candidates-size cur-phrase)))
      (if (or (= index (1- size)) (< index 0))
	  (setf (kgm-phrase-cur-candidate-index cur-phrase) 0)
	  (setf (kgm-phrase-cur-candidate-index cur-phrase) (1+ index)))
      (when (<= kgm-show-conversion-list-count
		(kgm-phrase-next-prev-counter cur-phrase))
	(kgm-show-conversion-list cur-phrase)))))


(defun kgm-romaji-prev-candidate ()
  (interactive)
  (let ((cur-phrase
	 (aref (kgm-conversion-phrases kgm-romaji-conversion)
	       kgm-romaji-current-phrase-index)))
    (incf (kgm-phrase-next-prev-counter cur-phrase))
    (let ((index (kgm-phrase-cur-candidate-index cur-phrase))
	  (size  (kgm-phrase-candidates-size     cur-phrase)))
      (if (<= index 0)
	  (setf (kgm-phrase-cur-candidate-index cur-phrase) (1- size))
	  (setf (kgm-phrase-cur-candidate-index cur-phrase) (1- index)))
      (when (<= kgm-show-conversion-list-count
		(kgm-phrase-next-prev-counter cur-phrase))
	(kgm-show-conversion-list cur-phrase)))))



(defun kgm-romaji-select-from-list ()
  (interactive)
  (let* ((selector-char last-input-event)
	 (index (position selector-char kgm-candidate-selector-chars)))
    (let ((cur-phrase
	   (aref (kgm-conversion-phrases kgm-romaji-conversion)
		 kgm-romaji-current-phrase-index)))
      ;まず、selectorがあるかを確認
      (when (kgm-phrase-candidate-selectors cur-phrase)
        (let ((cur-cand-index (kgm-phrase-cur-candidate-index cur-phrase))
	      (selectors      (kgm-phrase-candidate-selectors cur-phrase)))
          ;ミニバッファに表示されていたselector
	  (let* ((selector (kgm-get-selector cur-cand-index selectors))
		 (region   (kgm-selector-region selector))
		 (from (car region)) (to (cdr region)))
	      (let ((new-cand-index (+ from index)))
		(when (between new-cand-index from to) ;はみ出してないか
		  (setf (kgm-phrase-cur-candidate-index cur-phrase)
			new-cand-index)))))
	(kgm-show-conversion-list cur-phrase)))))


(defun kgm-romaji-accept ()
  (interactive)
  (setq kgm-romaji-converting nil))

(defun kgm-romaji-display-phrases (begin end delim-p)
  (goto-char begin)
  (delete-region begin end)
  (kgm-delete-all-overlays)
  (loop
   with delim      = (if delim-p kgm-preedit-delim-mark "")
   with begin-mark = (if delim-p kgm-preedit-begin-mark "")
   with phrases    = (kgm-conversion-phrases kgm-romaji-conversion)
   for phrase across phrases
   for i from 0
   for from      = (1+ (point))   ;begin-mark, delimのハイライトはしない
   for separator = (if (= i 0) begin-mark delim)
   for focusedp  = (= i kgm-romaji-current-phrase-index)
   do      (kgm-insert-phrase phrase from separator focusedp)
   finally (let ((current-phrase
		  (aref phrases kgm-romaji-current-phrase-index)))
	     (kgm-put-and-register-overlay begin (point) 'face 'underline)))
  (point))

(defun kgm-romaji-region (from to)
  (interactive "r")
  (let ((input (buffer-substring from to))
	(begin from) (end to))
    (setq kgm-romaji-conversion (kgm-make-romaji-conversion input))
    (setq kgm-romaji-current-phrase-index 0)
    (setq kgm-romaji-converting           t)

    (setq end (kgm-romaji-display-phrases begin end t))

    (unwind-protect
      (let ((current-input-method-title kgm-romaji-input-method-title)
	    (input-method-function nil))
	(while kgm-romaji-converting
	  (let* ((overriding-terminal-local-map kgm-romaji-keymap)
		 (keyseq (read-key-sequence nil))
		 (cmd (lookup-key kgm-romaji-keymap keyseq)))
	    (when (not (commandp cmd))
	      ; 変なキーが来た場合
	      ; 現在の変換をアクセプトして、変なキーを次の入力文字にする。
	      (setq unread-input-method-events
		    (append (string-to-list (this-single-command-raw-keys))
			    unread-input-method-events))
	      (setq cmd 'kgm-romaji-accept))
	    (call-interactively cmd)
	    (setq end (kgm-romaji-display-phrases
		        begin end (if (eq cmd 'kgm-romaji-accept) nil t)))))
	(- end begin)) ;これを返さないといけない
      (kgm-delete-all-overlays))))

(provide 'kgm-romaji)
