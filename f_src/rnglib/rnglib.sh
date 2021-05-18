#! /bin/bash
#
gfortran -c -Wall rnglib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv rnglib.o ~/lib/rnglib.o
#
echo "Normal end of execution."
