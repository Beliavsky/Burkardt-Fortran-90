#! /bin/bash
#
gfortran -c -Wall quadmom.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quadmom.o ~/lib/quadmom.o
#
echo "Normal end of execution."
