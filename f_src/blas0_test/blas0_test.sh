#! /bin/bash
#
gfortran -c -Wall blas0_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o blas0_test blas0_test.o $HOME/lib/blas0.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm blas0_test.o
#
./blas0_test > blas0_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm blas0_test
#
echo "Normal end of execution."
