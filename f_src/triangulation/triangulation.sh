#! /bin/bash
#
gfortran -c -Wall triangulation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangulation.o ~/lib/triangulation.o
#
echo "Normal end of execution."
