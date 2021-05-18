#! /bin/bash
#
gfortran -c -Wall -fopenmp prime_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp prime_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm prime_openmp.o
mv a.out $HOME/bin/prime_openmp
#
echo "Normal end of execution."
