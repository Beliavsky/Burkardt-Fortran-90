#! /bin/bash
#
gfortran -c -Wall legendre_product_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv legendre_product_polynomial.o ~/lib/legendre_product_polynomial.o
#
echo "Normal end of execution."
