#! /bin/bash
#
gfortran -c -Wall specfun.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv specfun.o ~/lib/specfun.o
#
echo "Normal end of execution."
