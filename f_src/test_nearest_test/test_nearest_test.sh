#! /bin/bash
#
$HOME/bin/test_nearest > test_nearest_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
