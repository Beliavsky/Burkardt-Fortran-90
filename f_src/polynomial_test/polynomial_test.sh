#! /bin/bash
#
gfortran -c -Wall polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran polynomial_test.o $HOME/lib/polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm polynomial_test.o
#
mv a.out polynomial_test
./polynomial_test > polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm polynomial_test
#
echo "Normal end of execution."
