#! /bin/bash
#
gfortran -c -Wall -fopenmp mxm_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp mxm_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm mxm_openmp.o
mv a.out $HOME/bin/mxm_openmp
#
echo "Normal end of execution."
