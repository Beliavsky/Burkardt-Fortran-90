#! /bin/bash
#
gfortran -c -Wall cyclic_reduction.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cyclic_reduction.o ~/lib/cyclic_reduction.o
#
echo "Normal end of execution."
