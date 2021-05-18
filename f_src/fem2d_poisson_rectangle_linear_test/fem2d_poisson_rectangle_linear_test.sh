#! /bin/bash
#
$HOME/bin/fem2d_poisson_rectangle_linear > fem2d_poisson_rectangle_linear_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
