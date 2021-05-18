#! /bin/bash
#
$HOME/bin/feynman_kac_1d > feynman_kac_1d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
