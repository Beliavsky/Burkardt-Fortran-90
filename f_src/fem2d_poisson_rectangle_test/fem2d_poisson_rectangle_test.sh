#! /bin/bash
#
$HOME/bin/fem2d_poisson_rectangle > fem2d_poisson_rectangle_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
