#! /bin/bash
#
gfortran -c -Wall test_interp_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_interp_2d.o ~/lib/test_interp_2d.o
#
echo "Normal end of execution."
