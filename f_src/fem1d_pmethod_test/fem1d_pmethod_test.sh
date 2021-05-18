#! /bin/bash
#
$HOME/bin/fem1d_pmethod > fem1d_pmethod_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
