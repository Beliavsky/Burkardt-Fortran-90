#! /bin/bash
#
$HOME/bin/sphere_voronoi gen_00010.xyz > sphere_voronoi_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
