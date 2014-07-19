#!/bin/bash

CC="../test/cc.s"
EXE="../src/bin/Debug/disasm.exe"
RESULT="myresult.txt"
ANSWER="answer.txt"

# 答えファイルがなければ作成
if [ ! -e $ANSWER ]; then
  cat $CC | > $ANSWER
fi
# 結果ファイルを作成
mono $EXE | > $RESULT
# diff を取って逆順で表示して色付け
diff -u $ANSWER $RESULT | gtac | colordiff

