#! /bin/bash
#
gfortran -c -Wall fem1d_lagrange_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o fem1d_lagrange_test fem1d_lagrange_test.o $HOME/lib/fem1d_lagrange.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem1d_lagrange_test.o
#
./fem1d_lagrange_test > fem1d_lagrange_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm fem1d_lagrange_test
#
echo "Normal end of execution."
