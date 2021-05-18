#! /bin/bash
#
gfortran -c -Wall test_nint.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_nint.o ~/lib/test_nint.o
#
echo "Normal end of execution."
