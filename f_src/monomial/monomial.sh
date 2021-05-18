#! /bin/bash
#
gfortran -c -Wall monomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv monomial.o ~/lib/monomial.o
#
echo "Normal end of execution."
