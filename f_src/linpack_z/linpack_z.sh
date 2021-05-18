#! /bin/bash
#
gfortran -c -Wall linpack_z.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv linpack_z.o ~/lib/linpack_z.o
#
echo "Normal end of execution."
