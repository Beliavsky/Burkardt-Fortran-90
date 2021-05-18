#! /bin/bash
#
gfortran -c -Wall test_interp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_interp.o ~/lib/test_interp.o
#
echo "Normal end of execution."
