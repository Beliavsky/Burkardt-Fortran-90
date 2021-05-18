#! /bin/bash
#
gfortran -c -Wall laupack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv laupack.o ~/lib/laupack.o
#
echo "Normal end of execution."
