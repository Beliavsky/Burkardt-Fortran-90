#! /bin/bash
#
gfortran -c -Wall blas2_z.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas2_z.o ~/lib/blas2_z.o
#
echo "Normal end of execution."
