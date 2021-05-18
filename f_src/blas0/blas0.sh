#! /bin/bash
#
gfortran -c -Wall blas0.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas0.o ~/lib/blas0.o
#
echo "Normal end of execution."
