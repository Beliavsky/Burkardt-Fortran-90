#! /bin/bash
#
gfortran -c -Wall linpack_d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv linpack_d.o ~/lib/linpack_d.o
#
echo "Normal end of execution."
