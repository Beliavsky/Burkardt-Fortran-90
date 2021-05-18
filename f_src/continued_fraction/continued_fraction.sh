#! /bin/bash
#
gfortran -c -Wall continued_fraction.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv continued_fraction.o ~/lib/continued_fraction.o
#
echo "Normal end of execution."
