#! /bin/bash
#
gfortran -c -Wall -fopenmp satisfy_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp satisfy_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm satisfy_openmp.o
mv a.out $HOME/bin/satisfy_openmp
#
echo "Normal end of execution."
