
;; KAGAMI��leim�Ȥ��ƻȤ������quail�Υѥå����������
;; quail/japanese��Ȥ�


(require 'quail)

(require 'kagami)


(setq quail-japanese-use-double-n t)

;; ���ߤ�����ʸ�����KAGAMI��ȤäƲ�̾�����򤸤�����Ѵ����롣
(defun quail-japanese-kanji-kgm ()
  (interactive)
  (when (= (char-before (overlay-end quail-conv-overlay)) ?n)
    ;; �Ǹ��ʸ����`n'�ξ�硣`��'���Ѵ����롣
    (goto-char (1- (overlay-end quail-conv-overlay)))
    (insert ?��)
    (delete-char 1))
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
	 (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
	(let ((result (kgm-region from (+ from len))))
	  (move-overlay quail-conv-overlay from (point))
	  (setq quail-conversion-str (buffer-substring from (point)))
	  (if (= (+ from result) (point))
	      (setq quail-converting nil))
	  (setq quail-translating nil))
      (set-marker from nil))))


(quail-define-package
  "japanese-kagami" "Japanese" "[KAGAMI]" nil
  "Japanese input method for using KAGAMI Kana Kanji Converter."
  nil t t nil nil nil nil nil
  'quail-japanese-update-translation
  '(("\C-t"  . quail-japanese-toggle-kana)
   (" "      . quail-japanese-kanji-kgm)
   ("\C-m"   . quail-no-conversion)
   ([return] . quail-no-conversion)))

(let ((transliteration-rules
       (append quail-japanese-transliteration-rules
	       '(("A" "��") ("B" "��") ("C" "��") ("D" "��") ("E" "��")
		 ("F" "��") ("G" "��") ("H" "��") ("I" "��") ("J" "��")
		 ("K" "��") ("L" "��") ("M" "��") ("N" "��") ("O" "��")
		 ("P" "��") ("Q" "��") ("R" "��") ("S" "��") ("T" "��")
		 ("U" "��") ("V" "��") ("W" "��") ("X" "��") ("Y" "��")
		 ("Z" "��"))
	       '(("la" "��") ("li" "��") ("lu" "��") ("le" "��") ("lo" "��")))))
  (dolist (elt transliteration-rules)
    (quail-defrule (car elt) (nth 1 elt))))



(require 'kgm-romaji)


(setq quail-japanese-switch-table
  '((?z . "japanese-zenkaku")
    (?k . "japanese-hankaku-kana")
    (?h . "japanese-kagami")
    (?q . ("japanese-romaji"))))


(defun quail-japanese-romaji ()
  (interactive)
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
	 (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
	(let ((result (kgm-romaji-region from (+ from len))))
	  (move-overlay quail-conv-overlay from (point))
	  (setq quail-conversion-str (buffer-substring from (point)))
	  (if (= (+ from result) (point))
	      (setq quail-converting nil))
	  (setq quail-translating nil))
      (set-marker from nil))))


(quail-define-package
 "japanese-romaji" "Japanese" "[ROMAJI]" nil
 "Temporary ASCII input mode used within the input method `japanese'.
  Type \"qq\" to go back to previous input method."
  nil t t nil nil nil nil nil
  nil
 '((" "      . quail-japanese-romaji)
   ("\C-m"   . quail-no-conversion)
   ([return] . quail-no-conversion))
 )


(let ((transliteration-rules 
       '(;(" " " ")
	 ("!" "!") ("\"" "\"") ("#" "#")
	 ("$" "$") ("%" "%") ("&" "&") ("'" "'")
	 ("(" "(") (")" ")") ("*" "*") ("+" "+")
	 ("," ",") ("-" "-") ("." ".") ("/" "/")
	 ("0" "0") ("1" "1") ("2" "2") ("3" "3")
	 ("4" "4") ("5" "5") ("6" "6") ("7" "7")
	 ("8" "8") ("9" "9") (":" ":") (";" ";")
	 ("<" "<") ("=" "=") (">" ">") ("?" "?")
	 ("@" "@") ("A" "A") ("B" "B") ("C" "C")
	 ("D" "D") ("E" "E") ("F" "F") ("G" "G")
	 ("H" "H") ("I" "I") ("J" "J") ("K" "K")
	 ("L" "L") ("M" "M") ("N" "N") ("O" "O")
	 ("P" "P") ("Q" "Q") ("R" "R") ("S" "S")
	 ("T" "T") ("U" "U") ("V" "V") ("W" "W")
	 ("X" "X") ("Y" "Y") ("Z" "Z") ("[" "[")
	 ("\\" "\\") ("]" "]") ("^" "^") ("_" "_")
	 ("`" "`") ("a" "a") ("b" "b") ("c" "c")
	 ("d" "d") ("e" "e") ("f" "f") ("g" "g")
	 ("h" "h") ("i" "i") ("j" "j") ("k" "k")
	 ("l" "l") ("m" "m") ("n" "n") ("o" "o")
	 ("p" "p") ("q" "q") ("r" "r") ("s" "s")
	 ("t" "t") ("u" "u") ("v" "v") ("w" "w")
	 ("x" "x") ("y" "y") ("z" "z") ("{" "{")
	 ("|" "|") ("}" "}") ("~" "~")
	 ("qq" quail-japanese-switch-package)
	 ("qh" quail-japanese-switch-package))))
  (dolist (elt transliteration-rules)
    (quail-defrule (car elt) (nth 1 elt))))
