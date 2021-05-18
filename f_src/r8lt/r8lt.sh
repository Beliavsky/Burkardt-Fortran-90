#! /bin/bash
#
gfortran -c -Wall r8lt.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8lt.o ~/lib/r8lt.o
#
echo "Normal end of execution."
