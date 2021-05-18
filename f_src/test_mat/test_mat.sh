#! /bin/bash
#
gfortran -c -Wall test_mat.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_mat.o ~/lib/test_mat.o
#
echo "Normal end of execution."
