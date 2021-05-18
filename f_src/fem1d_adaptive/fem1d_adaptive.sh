#! /bin/bash
#
gfortran -c -Wall fem1d_adaptive.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_adaptive.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem1d_adaptive.o
#
mv a.out ~/bin/fem1d_adaptive
#
echo "Normal end of execution."
