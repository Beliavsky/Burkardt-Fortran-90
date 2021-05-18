#! /bin/bash
#
gfortran -c -Wall bins.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bins.o ~/lib/bins.o
#
echo "Normal end of execution."
