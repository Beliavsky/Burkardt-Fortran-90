#! /bin/bash
#
gfortran -c -Wall etdrk4.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv etdrk4.o ~/lib/etdrk4.o
#
echo "Normal end of execution."
