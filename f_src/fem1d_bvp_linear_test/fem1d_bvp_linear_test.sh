#! /bin/bash
#
gfortran -c -Wall fem1d_bvp_linear_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_bvp_linear_test.o $HOME/lib/fem1d_bvp_linear.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem1d_bvp_linear_test.o
#
mv a.out fem1d_bvp_linear_test
./fem1d_bvp_linear_test > fem1d_bvp_linear_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem1d_bvp_linear_test
#
gnuplot < commands_h1.txt
gnuplot < commands_l2.txt
gnuplot < commands_mx.txt
#
echo "Normal end of execution."
