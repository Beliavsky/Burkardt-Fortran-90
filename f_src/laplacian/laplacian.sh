#! /bin/bash
#
gfortran -c -Wall laplacian.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv laplacian.o ~/lib/laplacian.o
#
echo "Normal end of execution."
