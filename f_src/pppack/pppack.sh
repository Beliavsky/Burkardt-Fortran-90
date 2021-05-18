#! /bin/bash
#
gfortran -c -Wall pppack.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pppack.o ~/lib/pppack.o
#
echo "Normal end of execution."
