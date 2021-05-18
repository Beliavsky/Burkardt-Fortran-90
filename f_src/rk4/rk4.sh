#! /bin/bash
#
gfortran -c -Wall rk4.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv rk4.o ~/lib/rk4.o
#
echo "Normal end of execution."
