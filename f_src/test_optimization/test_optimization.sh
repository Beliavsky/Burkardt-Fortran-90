#! /bin/bash
#
gfortran -c -Wall test_optimization.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_optimization.o ~/lib/test_optimization.o
#
echo "Normal end of execution."
