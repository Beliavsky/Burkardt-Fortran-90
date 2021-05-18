#! /bin/bash
#
gfortran -c -Wall test_int_2d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_int_2d.o ~/lib/test_int_2d.o
#
echo "Normal end of execution."
