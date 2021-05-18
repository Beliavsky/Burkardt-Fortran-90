#! /bin/bash
#
gfortran -c -Wall r8ut.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8ut.o ~/lib/r8ut.o
#
echo "Normal end of execution."
