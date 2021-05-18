#! /bin/bash
#
gfortran -c -Wall rkf45.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv rkf45.o ~/lib/rkf45.o
#
echo "Normal end of execution."
