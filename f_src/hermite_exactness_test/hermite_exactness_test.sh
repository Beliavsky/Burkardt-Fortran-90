#! /bin/bash
#
$HOME/bin/hermite_exactness hermite_probabilist_010 10 4 > hermite_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
