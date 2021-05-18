#! /bin/bash
#
gfortran -c -Wall stripack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv stripack.o ~/lib/stripack.o
#
echo "Normal end of execution."
