#! /bin/bash
#
gfortran -c -Wall -fopenmp heated_plate_openmp.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -fopenmp heated_plate_openmp.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm heated_plate_openmp.o
mv a.out $HOME/bin/heated_plate_openmp
#
echo "Normal end of execution."
