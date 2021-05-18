#! /bin/bash
#
gfortran -c -Wall brownian_motion_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv brownian_motion_simulation.o ~/lib/brownian_motion_simulation.o
#
echo "Normal end of execution."
