#! /bin/bash
#
gfortran -c -Wall hyperball_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hyperball_integrals.o ~/lib/hyperball_integrals.o
#
echo "Normal end of execution."
