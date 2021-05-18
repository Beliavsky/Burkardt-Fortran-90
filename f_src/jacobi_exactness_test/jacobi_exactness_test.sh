#! /bin/bash
#
$HOME/bin/jacobi_exactness jac_o2_a0.5_b1.5 5 0.5 1.5> jacobi_exactness_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
echo "Normal end of execution."
