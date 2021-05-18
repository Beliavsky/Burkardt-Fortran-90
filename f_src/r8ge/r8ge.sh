#! /bin/bash
#
gfortran -c -Wall r8ge.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r8ge.o ~/lib/r8ge.o
#
echo "Normal end of execution."
