#! /bin/bash
#
gfortran -c -Wall cube_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cube_integrals.o ~/lib/cube_integrals.o
#
echo "Normal end of execution."
