#!/bin/bash

EXE="../src/bin/Debug/disasm.exe"
RESULT="myresult.s"
ANSWER="answer.s"

# 引数がない場合は使い方を表示
if [ $# -eq 0 ]; then
  echo "Usage: ./spec.sh binary_file"
fi

# 答えファイルがなければ作成
if [ ! -e $ANSWER ]; then
  cat $1 > $ANSWER
fi

# 結果ファイルを作成
mono $EXE > $RESULT

# 差分を表示
colordiff $ANSWER $RESULT

