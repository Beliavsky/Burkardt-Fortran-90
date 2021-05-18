#! /bin/bash
#
$HOME/bin/sphere_cvt > sphere_cvt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
