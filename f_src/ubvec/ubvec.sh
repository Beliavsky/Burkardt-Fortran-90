#! /bin/bash
#
gfortran -c -Wall ubvec.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv ubvec.o ~/lib/ubvec.o
#
echo "Normal end of execution."
