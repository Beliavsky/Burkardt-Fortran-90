#! /bin/bash
#
gfortran -c -Wall test_matrix_exponential.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_matrix_exponential.o ~/lib/test_matrix_exponential.o
#
echo "Normal end of execution."
