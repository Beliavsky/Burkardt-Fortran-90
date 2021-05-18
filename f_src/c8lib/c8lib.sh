#! /bin/bash
#
gfortran -c -Wall c8lib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv c8lib.o ~/lib/c8lib.o
#
echo "Normal end of execution."
