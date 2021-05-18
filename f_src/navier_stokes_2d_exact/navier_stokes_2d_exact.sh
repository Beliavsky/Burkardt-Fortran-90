#! /bin/bash
#
gfortran -c -Wall navier_stokes_2d_exact.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv navier_stokes_2d_exact.o ~/lib/navier_stokes_2d_exact.o
#
echo "Normal end of execution."
