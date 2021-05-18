#! /bin/bash
#
gfortran -c -Wall fd1d_heat_explicit.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fd1d_heat_explicit.o ~/lib/fd1d_heat_explicit.o
#
echo "Normal end of execution."
