#! /bin/bash
#
gfortran -c -Wall shepard_interp_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv shepard_interp_1d.o ~/lib/shepard_interp_1d.o
#
echo "Normal end of execution."
