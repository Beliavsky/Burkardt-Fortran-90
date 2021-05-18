#! /bin/bash
#
gfortran -c -Wall test_eigen.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_eigen.o ~/lib/test_eigen.o
#
echo "Normal end of execution."
