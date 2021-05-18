#! /bin/bash
#
gfortran -c -Wall fem1d_pmethod.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d_pmethod.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm fem1d_pmethod.o
#
mv a.out ~/bin/fem1d_pmethod
#
echo "Normal end of execution."
