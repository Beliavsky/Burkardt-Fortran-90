#! /bin/bash
#
gfortran -c -Wall triangle01_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle01_integrals.o ~/lib/triangle01_integrals.o
#
echo "Normal end of execution."
