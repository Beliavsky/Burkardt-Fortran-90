#! /bin/bash
#
gfortran -c -Wall test_values.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_values.o ~/lib/test_values.o
#
echo "Normal end of execution."
