#! /bin/bash
#
$HOME/bin/subanagram 4 amsterdam > subanagram_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
