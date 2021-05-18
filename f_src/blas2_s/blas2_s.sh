#! /bin/bash
#
gfortran -c -Wall blas2_s.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blas2_s.o ~/lib/blas2_s.o
#
echo "Normal end of execution."
