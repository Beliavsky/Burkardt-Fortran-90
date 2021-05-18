#! /bin/bash
#
gfortran -c -Wall r8lib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8lib.o ~/lib/r8lib.o
#
echo "Normal end of execution."
