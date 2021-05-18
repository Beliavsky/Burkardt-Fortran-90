#! /bin/bash
#
gfortran -c -Wall bernstein_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bernstein_polynomial_test bernstein_polynomial_test.o $HOME/lib/bernstein_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bernstein_polynomial_test.o
#
./bernstein_polynomial_test > bernstein_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bernstein_polynomial_test
#
echo "Normal end of execution."
