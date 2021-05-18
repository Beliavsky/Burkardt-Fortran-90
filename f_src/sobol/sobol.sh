#! /bin/bash
#
gfortran -c -Wall sobol.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sobol.o ~/lib/sobol.o
#
echo "Normal end of execution."
