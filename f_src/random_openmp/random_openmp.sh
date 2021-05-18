#! /bin/bash
#
gfortran -c -Wall -fopenmp random_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp random_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm random_openmp.o
mv a.out $HOME/bin/random_openmp
#
echo "Normal end of execution."
