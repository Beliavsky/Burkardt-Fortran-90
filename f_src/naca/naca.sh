#! /bin/bash
#
gfortran -c -Wall naca.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv naca.o ~/lib/naca.o
#
echo "Normal end of execution."
