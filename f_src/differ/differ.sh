#! /bin/bash
#
gfortran -c -Wall differ.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv differ.o ~/lib/differ.o
#
echo "Normal end of execution."
