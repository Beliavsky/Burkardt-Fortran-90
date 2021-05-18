#! /bin/bash
#
gfortran -c -Wall gegenbauer_polynomial.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv gegenbauer_polynomial.o ~/lib/gegenbauer_polynomial.o
#
echo "Normal end of execution."
