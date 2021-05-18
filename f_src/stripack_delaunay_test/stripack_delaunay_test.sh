#! /bin/bash
#
$HOME/bin/stripack_delaunay sphere_grid_icos1_f1.xyz > stripack_delaunay_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
