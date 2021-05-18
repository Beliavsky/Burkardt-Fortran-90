#! /bin/bash
#
gfortran -c -Wall edge.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv edge.o ~/lib/edge.o
#
echo "Normal end of execution."
