#! /bin/bash
#
gfortran -c -Wall test_nonlin.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_nonlin.o ~/lib/test_nonlin.o
#
echo "Normal end of execution."
