#! /bin/bash
#
gfortran -c -Wall test_zero.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_zero.o ~/lib/test_zero.o
#
echo "Normal end of execution."
