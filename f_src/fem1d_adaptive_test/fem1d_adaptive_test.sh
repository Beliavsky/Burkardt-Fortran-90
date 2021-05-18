#! /bin/bash
#
$HOME/bin/fem1d_adaptive > fem1d_adaptive_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
