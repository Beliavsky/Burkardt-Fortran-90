#! /bin/bash
#
gfortran -c -Wall calpak.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv calpak.o ~/lib/calpak.o
#
echo "Normal end of execution."
