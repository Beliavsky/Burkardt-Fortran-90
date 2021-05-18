#! /bin/bash
#
gfortran -c -Wall fem1d_bvp_quadratic.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem1d_bvp_quadratic.o ~/lib/fem1d_bvp_quadratic.o
#
echo "Normal end of execution."
