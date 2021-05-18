#! /bin/bash
#
gfortran -c -Wall linpack_d_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o linpack_d_test linpack_d_test.o $HOME/lib/linpack_d.o $HOME/lib/blas1_d.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm linpack_d_test.o
#
./linpack_d_test > linpack_d_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm linpack_d_test
#
echo "Normal end of execution."
