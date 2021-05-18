#! /bin/bash
#
gfortran -c -Wall fem1d_nonlinear.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_nonlinear.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem1d_nonlinear.o
#
mv a.out ~/bin/fem1d_nonlinear
#
echo "Normal end of execution."
