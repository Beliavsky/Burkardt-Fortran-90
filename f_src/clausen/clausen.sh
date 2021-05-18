#! /bin/bash
#
gfortran -c -Wall clausen.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv clausen.o ~/lib/clausen.o
#
echo "Normal end of execution."
