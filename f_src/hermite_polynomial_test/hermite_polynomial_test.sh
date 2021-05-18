#! /bin/bash
#
gfortran -c -Wall hermite_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
fi
#
gfortran hermite_polynomial_test.o $HOME/lib/hermite_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_polynomial_test.o
#
mv a.out hermite_polynomial_test
./hermite_polynomial_test > hermite_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hermite_polynomial_test
#
echo "Normal end of execution."
