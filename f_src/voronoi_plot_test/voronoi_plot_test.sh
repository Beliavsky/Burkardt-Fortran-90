#! /bin/bash
#
$HOME/bin/voronoi_plot diamond_02_00009.txt 2 > voronoi_plot_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."

