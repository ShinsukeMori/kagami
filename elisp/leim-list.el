(register-input-method
  "japanese-kagami" "Japanese" 'quail-use-package
  "[KAGAMI]" "Japanese input method using KAGAMI Kana Kanji Converter"
  ; quail-use-pacakge������ɤ߹��ޤ��饤�֥�ꡣ
  ; quail/japanese�򤢤꤬�����Ȥ碌�Ƥ���������
  ; ���θ塢japanese-kagami��quail�ѥå���������ɡ�
  ; �������ơ�japanese-kagami�������ƥ��֤ˤʤ롣
  "quail/japanese" "~/kagami/elisp/kagami-quail-package")

;; ����ץåȥ᥽�åɤ�KAGAMI�ˤ��롣
(set-language-info "Japanese" 'input-method "japanese-kagami")
