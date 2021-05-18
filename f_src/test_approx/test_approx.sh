#! /bin/bash
#
gfortran -c -Wall test_approx.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_approx.o ~/lib/test_approx.o
#
echo "Normal end of execution."
