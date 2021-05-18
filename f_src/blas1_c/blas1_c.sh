#! /bin/bash
#
gfortran -c -Wall blas1_c.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas1_c.o ~/lib/blas1_c.o
#
echo "Normal end of execution."
