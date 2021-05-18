#! /bin/bash
#
$HOME/bin/football_scores > football_scores_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

