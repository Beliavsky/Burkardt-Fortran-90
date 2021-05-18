#! /bin/bash
#
$HOME/bin/md 3 500 5000 0.01 > md_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
