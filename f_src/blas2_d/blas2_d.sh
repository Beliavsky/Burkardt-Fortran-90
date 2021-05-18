#! /bin/bash
#
gfortran -c -Wall blas2_d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas2_d.o ~/lib/blas2_d.o
#
echo "Normal end of execution."
