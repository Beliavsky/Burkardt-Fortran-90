#! /bin/bash
#
$HOME/bin/memory 10 27 > memory_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
