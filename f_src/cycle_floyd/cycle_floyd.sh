#! /bin/bash
#
gfortran -c -Wall cycle_floyd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cycle_floyd.o ~/lib/cycle_floyd.o
#
echo "Normal end of execution."
