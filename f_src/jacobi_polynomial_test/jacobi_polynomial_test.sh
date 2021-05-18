#! /bin/bash
#
gfortran -c -Wall jacobi_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran jacobi_polynomial_test.o $HOME/lib/jacobi_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jacobi_polynomial_test.o
#
mv a.out jacobi_polynomial_test
./jacobi_polynomial_test > jacobi_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm jacobi_polynomial_test
#
echo "Normal end of execution."
