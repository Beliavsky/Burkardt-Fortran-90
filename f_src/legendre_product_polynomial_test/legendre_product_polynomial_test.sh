#! /bin/bash
#
gfortran -c -Wall legendre_product_polynomial_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o legendre_product_polynomial_test legendre_product_polynomial_test.o $HOME/lib/legendre_product_polynomial.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm legendre_product_polynomial_test.o
#
./legendre_product_polynomial_test > legendre_product_polynomial_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm legendre_product_polynomial_test
#
echo "Normal end of execution."
