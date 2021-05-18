#! /bin/bash
#
gfortran -c -Wall -fopenmp schedule_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp schedule_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm schedule_openmp.o
mv a.out $HOME/bin/schedule_openmp
#
echo "Normal end of execution."
