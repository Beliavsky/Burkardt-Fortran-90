#! /bin/bash
#
gfortran -c -Wall -fopenmp fft_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp fft_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fft_openmp.o
mv a.out $HOME/bin/fft_openmp
#
echo "Normal end of execution."
