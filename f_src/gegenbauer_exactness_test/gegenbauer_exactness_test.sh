#! /bin/bash
#
$HOME/bin/gegenbauer_exactness gegen_o8_a0.5 18 0.5 > gegenbauer_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
