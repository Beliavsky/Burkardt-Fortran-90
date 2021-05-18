#! /bin/bash
#
gfortran -c -Wall lobatto_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o lobatto_polynomial_test lobatto_polynomial_test.o \
  $HOME/lib/lobatto_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm lobatto_polynomial_test.o
#
./lobatto_polynomial_test > lobatto_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm lobatto_polynomial_test
#
echo "Normal end of execution."
