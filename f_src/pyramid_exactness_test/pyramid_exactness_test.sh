#! /bin/bash
#
$HOME/bin/pyramid_exactness pyramid_l3x3_j3 7 > pyramid_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
