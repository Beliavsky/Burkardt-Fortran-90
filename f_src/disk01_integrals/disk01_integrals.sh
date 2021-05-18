#! /bin/bash
#
gfortran -c -Wall disk01_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv disk01_integrals.o ~/lib/disk01_integrals.o
#
echo "Normal end of execution."
