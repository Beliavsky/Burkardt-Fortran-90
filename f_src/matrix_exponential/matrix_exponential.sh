#! /bin/bash
#
gfortran -c -Wall matrix_exponential.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv matrix_exponential.o ~/lib/matrix_exponential.o
#
echo "Normal end of execution."
