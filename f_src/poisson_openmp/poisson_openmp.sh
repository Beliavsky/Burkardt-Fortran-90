#! /bin/bash
#
gfortran -c -Wall -fopenmp poisson_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp poisson_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm poisson_openmp.o
mv a.out $HOME/bin/poisson_openmp
#
echo "Normal end of execution."
