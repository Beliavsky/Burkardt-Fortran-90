#! /bin/bash
#
gfortran -c -Wall pwl_product_integral_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o pwl_product_integral_test pwl_product_integral_test.o $HOME/lib/pwl_product_integral.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm pwl_product_integral_test.o
#
./pwl_product_integral_test > pwl_product_integral_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm pwl_product_integral_test
#
echo "Normal end of execution."
