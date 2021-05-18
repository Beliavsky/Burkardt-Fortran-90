#! /bin/bash
#
gfortran -c -Wall cuda_loop.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cuda_loop.o ~/lib/cuda_loop.o
#
echo "Normal end of execution."
