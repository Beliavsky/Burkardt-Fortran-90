#! /bin/bash
#
gfortran -c -Wall wedge_integrals.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv wedge_integrals.o ~/lib/wedge_integrals.o
#
echo "Normal end of execution."
