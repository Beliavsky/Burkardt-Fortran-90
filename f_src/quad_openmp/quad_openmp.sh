#! /bin/bash
#
gfortran -c -Wall -fopenmp quad_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp quad_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad_openmp.o
mv a.out $HOME/bin/quad_openmp
#
echo "Normal end of execution."
