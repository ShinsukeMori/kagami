# Kagami

## Overview
Japanese input method (client and server)
* a client for Emacs
* a server in c++
* a model suitable for technical writing

We will soon write an install manual.

## Settings
!! There are still some incompleteness in the files on git !!
!! We are updating the files !!
- Inflate the zip file and move kagami/ to ~/.kagami/
- Add below to your .emacs
```
(setq load-path (cons "~/kagami/elisp/" load-path))
(load "~/kagami/elisp/leim-list.el")
```

## Reference

##### Statistical Input Method based on a Phrase Class n-gram Model
* Hirokuni Maeta, Shinsuke Mori 
* WTIM, pp.1-13, 2012.
* http://plata.ar.media.kyoto-u.ac.jp/mori/research/public/maeta-WTIM12.pdf

##### 学術論文執筆のための仮名漢字変換システム
* 高橋 文彦, 前田 浩邦, 森 信介 
* 言語処理学会第21回年次大会, 2015. 
* http://plata.ar.media.kyoto-u.ac.jp/mori/research/public/takahasi-NLP15-KKC.pdf
* http://plata.ar.media.kyoto-u.ac.jp/mori/research/public/slides/NLP15-KKC.pdf
    
