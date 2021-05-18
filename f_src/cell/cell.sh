#! /bin/bash
#
gfortran -c -Wall cell.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cell.o ~/lib/cell.o
#
echo "Normal end of execution."
