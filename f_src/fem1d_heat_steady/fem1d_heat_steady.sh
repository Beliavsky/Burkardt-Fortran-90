#! /bin/bash
#
gfortran -c -Wall fem1d_heat_steady.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem1d_heat_steady.o ~/lib/fem1d_heat_steady.o
#
echo "Normal end of execution."
