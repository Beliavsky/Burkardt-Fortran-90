#! /bin/bash
#
gfortran -c -Wall -I/$HOME/include hankel_cholesky_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hankel_cholesky_test hankel_cholesky_test.o /$HOME/lib/hankel_cholesky.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
#
rm hankel_cholesky_test.o
#
./hankel_cholesky_test > hankel_cholesky_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hankel_cholesky_test
#
echo "Normal end of execution."
