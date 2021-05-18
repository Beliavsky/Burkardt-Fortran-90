#! /bin/bash
#
gfortran -c -Wall multigrid_poisson_1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv multigrid_poisson_1d.o ~/lib/multigrid_poisson_1d.o
#
echo "Normal end of execution."
