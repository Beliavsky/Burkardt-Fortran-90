#! /bin/bash
#
gfortran -c -Wall hypercube_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hypercube_integrals.o ~/lib/hypercube_integrals.o
#
echo "Normal end of execution."
