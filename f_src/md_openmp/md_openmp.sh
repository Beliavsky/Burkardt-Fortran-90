#! /bin/bash
#
gfortran -c -Wall -fopenmp md_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp md_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm md_openmp.o
mv a.out $HOME/bin/md_openmp
#
echo "Normal end of execution."
