#! /bin/bash
#
gfortran -c -Wall blas3_z.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas3_z.o ~/lib/blas3_z.o
#
echo "Normal end of execution."
