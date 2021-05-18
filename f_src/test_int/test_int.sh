#! /bin/bash
#
gfortran -c -Wall test_int.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_int.o ~/lib/test_int.o
#
echo "Normal end of execution."
