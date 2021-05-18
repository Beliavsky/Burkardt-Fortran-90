#! /bin/bash
#
gfortran -c -Wall chebyshev_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o chebyshev_polynomial_test chebyshev_polynomial_test.o $HOME/lib/chebyshev_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm chebyshev_polynomial_test.o
#
./chebyshev_polynomial_test > chebyshev_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm chebyshev_polynomial_test
#
echo "Normal end of execution."
