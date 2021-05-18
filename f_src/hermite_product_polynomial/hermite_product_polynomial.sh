#! /bin/bash
#
gfortran -c -Wall hermite_product_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hermite_product_polynomial.o ~/lib/hermite_product_polynomial.o
#
echo "Normal end of execution."
