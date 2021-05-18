#! /bin/bash
#
gfortran -c -Wall polygon_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polygon_integrals.o ~/lib/polygon_integrals.o
#
echo "Normal end of execution."
