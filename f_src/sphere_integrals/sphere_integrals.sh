#! /bin/bash
#
gfortran -c -Wall sphere_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_integrals.o ~/lib/sphere_integrals.o
#
echo "Normal end of execution."
