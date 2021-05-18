#! /bin/bash
#
gfortran -c -Wall -fopenmp jacobi_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp jacobi_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm jacobi_openmp.o
mv a.out $HOME/bin/jacobi_openmp
#
echo "Normal end of execution."
