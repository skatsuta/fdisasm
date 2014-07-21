#!/bin/bash

EXE="../src/bin/Debug/disasm.exe"
RESULT="myresult.s"
ANSWER=$1

# 引数がない場合は使い方を表示
if [ $# -eq 0 ]; then
  echo "Usage: ./spec.sh answer_file"
fi

# 結果ファイルを作成
mono $EXE > $RESULT

# 差分を表示
colordiff $ANSWER $RESULT

