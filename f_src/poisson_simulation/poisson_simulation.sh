#! /bin/bash
#
gfortran -c -Wall poisson_simulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv poisson_simulation.o ~/lib/poisson_simulation.o
#
echo "Normal end of execution."
