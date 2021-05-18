#! /bin/bash
#
gfortran -c -Wall blas3_d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas3_d.o ~/lib/blas3_d.o
#
echo "Normal end of execution."
