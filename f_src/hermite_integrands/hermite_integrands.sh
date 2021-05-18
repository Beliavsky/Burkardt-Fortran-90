#! /bin/bash
#
gfortran -c -Wall hermite_integrands.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv hermite_integrands.o ~/lib/hermite_integrands.o
#
echo "Normal end of execution."
