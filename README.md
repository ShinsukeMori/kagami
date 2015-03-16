# Kagami
Japanese input method (client and server)


## Build
- cd && git clone https://github.com/ShinsukeMori/kagami
- cd kagami
- mkdir log bin
- cd src
- make
- mv ./server ../bin

## Settings
add below to your .emacs
```
(setq load-path (cons "~/kagami/elisp/" load-path))
(load "~/kagami/elisp/leim-list.el")
```

## Requirements
- x86 (not x86-64) architectures only
- Encoding must be euc-jp

## Reference
Statistical Input Method based on a Phrase Class n-gram Model
    Hirokuni Maeta, Shinsuke Mori 
    WTIM, pp.1-13, 2012.
    http://plata.ar.media.kyoto-u.ac.jp/mori/research/public/maeta-WTIM12.pdf
