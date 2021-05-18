#! /bin/bash
#
gfortran -c -Wall -fopenmp ziggurat_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp ziggurat_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm ziggurat_openmp.o
mv a.out $HOME/bin/ziggurat_openmp
#
echo "Normal end of execution."

