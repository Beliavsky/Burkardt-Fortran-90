#! /bin/bash
#
gfortran -c -Wall cdflib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cdflib.o ~/lib/cdflib.o
#
echo "Normal end of execution."
