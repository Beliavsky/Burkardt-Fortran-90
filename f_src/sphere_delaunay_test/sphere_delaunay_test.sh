#! /bin/bash
#
$HOME/bin/sphere_delaunay gen_00100.xyz > sphere_delaunay_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
