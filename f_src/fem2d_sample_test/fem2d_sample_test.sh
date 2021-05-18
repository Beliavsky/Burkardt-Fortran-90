#! /bin/bash
#
$HOME/bin/fem2d_sample fem_sq sample_sq > fem2d_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
