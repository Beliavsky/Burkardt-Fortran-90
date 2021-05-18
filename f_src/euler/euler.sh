#! /bin/bash
#
gfortran -c -Wall euler.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv euler.o ~/lib/euler.o
#
echo "Normal end of execution."
