#! /bin/bash
#
gfortran -c -Wall bisection_integer.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bisection_integer.o ~/lib/bisection_integer.o
#
echo "Normal end of execution."
