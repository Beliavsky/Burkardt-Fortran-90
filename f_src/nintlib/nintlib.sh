#! /bin/bash
#
gfortran -c -Wall nintlib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv nintlib.o ~/lib/nintlib.o
#
echo "Normal end of execution."
