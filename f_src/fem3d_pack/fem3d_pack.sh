#! /bin/bash
#
gfortran -c -Wall fem3d_pack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem3d_pack.o ~/lib/fem3d_pack.o
#
echo "Normal end of execution."
