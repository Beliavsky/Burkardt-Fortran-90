#! /bin/bash
#
gfortran -c -Wall luhn.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv luhn.o ~/lib/luhn.o
#
echo "Normal end of execution."
