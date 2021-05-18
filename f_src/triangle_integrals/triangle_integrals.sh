#! /bin/bash
#
gfortran -c -Wall triangle_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_integrals.o ~/lib/triangle_integrals.o
#
echo "Normal end of execution."
