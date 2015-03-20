(require 'kgm-util)
(require 'kgm-data)

(defun kgm-conv->form (conversion)
  (let ((cur-phr-idx (kgm-conv-cur-phr-idx conversion)))
    (if (minusp cur-phr-idx)
	;変換がキャンセルされて、すべて平仮名になった場合
	(list (list (kgm-conv-string conversion)
		    (kgm-conv-string conversion)
		    "C"))
        (loop for phrase across (kgm-conv-phrases conversion)
	      for cand = (kgm-get-cur-cand phrase)
	      collect (if cand ;通常
			  (list (kgm-cand-string cand)
				(kgm-phr-string phrase)
				(symbol-name (kgm-cand-origin cand)))
			  (case (kgm-phr-cur-cand-idx phrase)
			    (-1 (list (kgm-phr-string phrase) ;ひらがな
				      (kgm-phr-string phrase)
				      "H")) 
			    (-2 (list (hiragana->katakana     ;カタカナ
				        (kgm-phr-string phrase))
				      (kgm-phr-string phrase)
				      "K"))))))))

(defun kgm-create-new-log (first-conv fixed-conv)
  (let ((current-time (current-time)))
    (let ((times (list (number-to-string (car  current-time))
		       (number-to-string (cadr current-time))))
	  (first-form (kgm-conv->form first-conv))
	  (fixed-form (kgm-conv->form fixed-conv)))
      (make-kgm-log :times times
		    :first-conv-form first-form
		    :fixed-conv-form fixed-form))))

(defun kgm-log->string (log)
  (flet ((form->flatten-list (form)
	   (mapcar #'(lambda (contents)
		       (destructuring-bind (conv kana org) contents
			 (concat conv "/" kana "/" org " ")))
		   form)))
    (let ((times (kgm-log-times log))
	  (first (kgm-log-first-conv-form log))
	  (fixed (kgm-log-fixed-conv-form log)))
      (let ((list (append (list (concat (car  times) " ")
				(concat (cadr times) "\n"))
			  (form->flatten-list first) (list "\n")
			  (form->flatten-list fixed) (list "\n")
			  (list "\n"))))
	(apply #'concat list)))))

(defun kgm-write-log (log-buffer log-list log-file)
  (interactive)
  (when log-file
    (let ((old-buf (current-buffer)))
      (set-buffer (get-buffer-create log-buffer))
      (erase-buffer)
      (dolist (log log-list)
        (insert (kgm-log->string log)))
      (append-to-file (point-min) (point-max) log-file)
      (set-buffer old-buf))))

(provide 'kgm-log)
