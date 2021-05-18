#! /bin/bash
#
gfortran -c -Wall solve.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv solve.o ~/lib/solve.o
#
echo "Normal end of execution."
