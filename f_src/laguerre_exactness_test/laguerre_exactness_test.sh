#! /bin/bash
#
$HOME/bin/laguerre_exactness lag_o04 10 0 > laguerre_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

