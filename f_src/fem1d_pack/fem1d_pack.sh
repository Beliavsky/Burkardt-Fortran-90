#! /bin/bash
#
gfortran -c -Wall fem1d_pack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fem1d_pack.o ~/lib/fem1d_pack.o
#
echo "Normal end of execution."
