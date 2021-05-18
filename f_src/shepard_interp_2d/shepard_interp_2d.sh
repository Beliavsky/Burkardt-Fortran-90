#! /bin/bash
#
gfortran -c -Wall shepard_interp_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv shepard_interp_2d.o ~/lib/shepard_interp_2d.o
#
echo "Normal end of execution."
