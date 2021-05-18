#! /bin/bash
#
gfortran -c -Wall fem2d_bvp_serene.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem2d_bvp_serene.o ~/lib/fem2d_bvp_serene.o
#
echo "Normal end of execution."
