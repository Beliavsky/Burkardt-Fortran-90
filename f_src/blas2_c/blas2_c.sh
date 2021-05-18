#! /bin/bash
#
gfortran -c -Wall blas2_c.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas2_c.o ~/lib/blas2_c.o
#
echo "Normal end of execution."
