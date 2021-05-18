#! /bin/bash
#
$HOME/bin/gen_laguerre_exactness gen_lag_o4_a0.5 10 0.5 0 > gen_laguerre_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
