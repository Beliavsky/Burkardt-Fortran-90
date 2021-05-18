#! /bin/bash
#
gfortran -c -Wall r83.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv r83.o ~/lib/r83.o
#
echo "Normal end of execution."
