#! /bin/bash
#
gfortran -c -Wall monomial_value.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv monomial_value.o ~/lib/monomial_value.o
#
echo "Normal end of execution."
