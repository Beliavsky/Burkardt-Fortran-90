#! /bin/bash
#
gfortran -c -Wall fem2d_pack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem2d_pack.o ~/lib/fem2d_pack.o
#
echo "Normal end of execution."
