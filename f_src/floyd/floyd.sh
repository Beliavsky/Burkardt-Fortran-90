#! /bin/bash
#
gfortran -c -Wall floyd.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv floyd.o ~/lib/floyd.o
#
echo "Normal end of execution."
