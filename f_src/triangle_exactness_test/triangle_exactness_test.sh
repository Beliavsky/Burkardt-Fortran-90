#! /bin/bash
#
$HOME/bin/triangle_exactness strang5 5 > triangle_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

