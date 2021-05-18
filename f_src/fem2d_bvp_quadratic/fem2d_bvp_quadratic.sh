#! /bin/bash
#
gfortran -c -Wall fem2d_bvp_quadratic.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem2d_bvp_quadratic.o ~/lib/fem2d_bvp_quadratic.o
#
echo "Normal end of execution."
