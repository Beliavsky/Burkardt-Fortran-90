#! /bin/bash
#
gfortran -c -Wall stokes_2d_exact.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv stokes_2d_exact.o ~/lib/stokes_2d_exact.o
#
echo "Normal end of execution."
