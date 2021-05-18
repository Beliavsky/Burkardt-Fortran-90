#! /bin/bash
#
$HOME/bin/fem1d > fem1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
