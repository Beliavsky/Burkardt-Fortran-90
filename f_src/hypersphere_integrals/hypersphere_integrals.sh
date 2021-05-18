#! /bin/bash
#
gfortran -c -Wall hypersphere_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypersphere_integrals.o ~/lib/hypersphere_integrals.o
#
echo "Normal end of execution."
