#! /bin/bash
#
gfortran -c -Wall test_lls.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_lls.o ~/lib/test_lls.o
#
echo "Normal end of execution."
