#! /bin/bash
#
$HOME/bin/fem3d_sample fem_sq sample_sq > fem3d_sample_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
