#! /bin/bash
#
gfortran -c -Wall blas3_s.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas3_s.o ~/lib/blas3_s.o
#
echo "Normal end of execution."
