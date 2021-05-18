#! /bin/bash
#
$HOME/bin/legendre_exactness leg_o4 10 > legendre_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

