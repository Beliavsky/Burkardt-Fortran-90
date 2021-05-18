#! /bin/bash
#
gfortran -c -Wall fsolve.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fsolve.o ~/lib/fsolve.o
#
echo "Normal end of execution."
