#! /bin/bash
#
gfortran -c -Wall laguerre_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran laguerre_polynomial_test.o $HOME/lib/laguerre_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm laguerre_polynomial_test.o
#
mv a.out laguerre_polynomial_test
./laguerre_polynomial_test > laguerre_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm laguerre_polynomial_test
#
echo "Normal end of execution."
