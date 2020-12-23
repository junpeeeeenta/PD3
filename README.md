# PD3
Common Lisp programs for PD3
## SBCLのインストール on ubuntu

$ sudo apt install sbcl

### sbclライブラリsb-aclreplを要求する

$ sbcl
* (require :sb-aclrepl)
CL-USER(1) :pwd
CL-USER(2) :cd
CL-USER(3) :cd common-lisp/pd3

## PD3システムのロードとdrawioファイルの取り込み

CL-USER(4) :ld pd3.asd
CL-USER(5) (asdf:load-system :pd3)
CL-USER(6) (in-package :pd3)
PD3(7) (read-drawio-file "PD3プラントエンジニア例題_作業プロセス記述.xml")

## PD3のコマンドインターフェースの使い方

PD3(8) (??)
> show all actions
> show all containers
> show all flows
> show all action values
> show actions related to <日本語>
> show flows related to <日本語>
> show containers related to <日本語>
> show <ID1> <ID2> <ID3> . . .
> verify112
> verify113
> verify1312
> verify1313
> verify1314
> exit
PD3(9) :exit
$ exit
