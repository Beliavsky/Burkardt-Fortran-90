#! /bin/bash
#
gfortran -c -Wall blas3_c.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas3_c.o ~/lib/blas3_c.o
#
echo "Normal end of execution."
