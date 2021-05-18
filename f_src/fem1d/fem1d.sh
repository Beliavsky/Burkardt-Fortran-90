#! /bin/bash
#
gfortran -c -Wall fem1d.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran fem1d.o
if [ $? -ne 0 ]; then
  echo "Errors while loading fem1d.o"
  exit
fi
rm fem1d.o
#
mv a.out ~/bin/fem1d
#
echo "Normal end of execution."
