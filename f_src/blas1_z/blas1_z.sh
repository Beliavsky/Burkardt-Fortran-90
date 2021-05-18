#! /bin/bash
#
gfortran -c -Wall blas1_z.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas1_z.o ~/lib/blas1_z.o
#
echo "Normal end of execution."
