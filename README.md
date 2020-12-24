# PD3
Common Lisp programs for PD3
## SBCLのインストール on ubuntu

$ sudo apt install sbcl

### sbclライブラリsb-aclreplを要求する

$ sbcl<br>
\* (require :sb-aclrepl)<br>
CL-USER(1) :pwd<br>
CL-USER(2) :cd<br>
CL-USER(3) :cd common-lisp/pd3<br>

## PD3システムのロードとdrawioファイルの取り込み

CL-USER(4) :ld pd3.asd<br>
CL-USER(5) (asdf:load-system :pd3)<br>
CL-USER(6) (in-package :pd3)<br>
PD3(7) (read-drawio-file "PD3プラントエンジニア例題_作業プロセス記述.xml")<br>

## PD3のコマンドインターフェースの使い方

PD3(8) (??)<br>
\> show all actions<br>
\> show all containers<br>
\> show all flows<br>
\> show all action values<br>
\> show actions related to <日本語><br>
\> show flows related to <日本語><br>
\> show containers related to <日本語><br>
\> show <ID1> <ID2> <ID3> . . .<br>
\> verify112<br>
\> verify113<br>
\> verify1312<br>
\> verify1313<br>
\> verify1314<br>
\> exit<br>
PD3(9) :exit<br>
$ exit<br>
