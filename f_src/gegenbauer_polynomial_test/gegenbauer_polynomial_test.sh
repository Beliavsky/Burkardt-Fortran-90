#! /bin/bash
#
gfortran -c -Wall gegenbauer_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gfortran -o gegenbauer_polynomial_test gegenbauer_polynomial_test.o $HOME/lib/gegenbauer_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gegenbauer_polynomial_test.o
#
./gegenbauer_polynomial_test > gegenbauer_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm gegenbauer_polynomial_test
#
echo "Normal end of execution."
