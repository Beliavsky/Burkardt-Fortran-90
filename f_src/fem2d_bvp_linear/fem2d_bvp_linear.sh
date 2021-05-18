#! /bin/bash
#
gfortran -c -Wall fem2d_bvp_linear.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem2d_bvp_linear.o ~/lib/fem2d_bvp_linear.o
#
echo "Normal end of execution."
