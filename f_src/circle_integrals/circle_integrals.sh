#! /bin/bash
#
gfortran -c -Wall circle_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv circle_integrals.o ~/lib/circle_integrals.o
#
echo "Normal end of execution."
