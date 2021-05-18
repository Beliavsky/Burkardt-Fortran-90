#! /bin/bash
#
$HOME/bin/fem1d_nonlinear > fem1d_nonlinear_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
