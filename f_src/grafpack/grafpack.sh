#! /bin/bash
#
gfortran -c -Wall grafpack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv grafpack.o ~/lib/grafpack.o
#
echo "Normal end of execution."
