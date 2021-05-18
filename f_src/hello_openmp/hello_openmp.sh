#! /bin/bash
#
gfortran -c -Wall -fopenmp hello_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp hello_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm hello_openmp.o
mv a.out $HOME/bin/hello_openmp
#
echo "Normal end of execution."
