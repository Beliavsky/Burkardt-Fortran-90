#! /bin/bash
#
$HOME/bin/stripack_voronoi f1.xyz > stripack_voronoi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
