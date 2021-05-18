#! /bin/bash
#
$HOME/bin/fem2d_heat_rectangle > fem2d_heat_rectangle_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
