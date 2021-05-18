#! /bin/bash
#
gfortran -c -Wall random_sorted.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv random_sorted.o ~/lib/random_sorted.o
#
echo "Normal end of execution."
