#! /bin/bash
#
gfortran -c -Wall combo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv combo.o ~/lib/combo.o
#
echo "Normal end of execution."
