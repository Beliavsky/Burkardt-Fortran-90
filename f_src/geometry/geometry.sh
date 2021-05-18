#! /bin/bash
#
gfortran -c -Wall geometry.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv geometry.o ~/lib/geometry.o
#
echo "Normal end of execution."
