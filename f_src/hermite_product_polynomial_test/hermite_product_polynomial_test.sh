#! /bin/bash
#
gfortran -c -Wall hermite_product_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o hermite_product_polynomial_test hermite_product_polynomial_test.o $HOME/lib/hermite_product_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hermite_product_polynomial_test.o
#
./hermite_product_polynomial_test > hermite_product_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm hermite_product_polynomial_test
#
echo "Normal end of execution."
