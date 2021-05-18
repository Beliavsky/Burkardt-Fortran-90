#! /bin/bash
#
$HOME/bin/hypercube_exactness cce_d2_level2 6 > hypercube_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
