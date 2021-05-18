#! /bin/bash
#
gfortran -c -Wall fem1d_bvp_linear.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem1d_bvp_linear.o ~/lib/fem1d_bvp_linear.o
#
echo "Normal end of execution."
