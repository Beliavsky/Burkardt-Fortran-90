#! /bin/bash
#
gfortran -c -Wall fd1d_heat_steady.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fd1d_heat_steady.o ~/lib/fd1d_heat_steady.o
#
echo "Normal end of execution."
