#! /bin/bash
#
gfortran -c -Wall pwl_product_integral.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pwl_product_integral.o ~/lib/pwl_product_integral.o
#
echo "Normal end of execution."
