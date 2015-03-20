(register-input-method
  "japanese-kagami" "Japanese" 'quail-use-package
  "[KAGAMI]" "Japanese input method using KAGAMI Kana Kanji Converter"
  ; quail-use-pacakgeの中で読み込まれるライブラリ。
  ; quail/japaneseをありがたく使わせていただく。
  ; その後、japanese-kagamiのquailパッケージをロード。
  ; そうして、japanese-kagamiがアクティブになる。
  "quail/japanese" "~/kagami/elisp/kagami-quail-package")

;; インプットメソッドをKAGAMIにする。
(set-language-info "Japanese" 'input-method "japanese-kagami")
