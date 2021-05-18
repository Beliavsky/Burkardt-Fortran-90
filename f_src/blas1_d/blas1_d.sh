#! /bin/bash
#
gfortran -c -Wall blas1_d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas1_d.o ~/lib/blas1_d.o
#
echo "Normal end of execution."
