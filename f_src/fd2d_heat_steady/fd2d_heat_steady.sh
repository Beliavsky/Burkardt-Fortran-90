#! /bin/bash
#
gfortran -c -Wall fd2d_heat_steady.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fd2d_heat_steady.o ~/lib/fd2d_heat_steady.o
#
echo "Normal end of execution."
