#! /bin/bash
#
gfortran -c -Wall mgmres.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv mgmres.o ~/lib/mgmres.o
#
echo "Normal end of execution."
