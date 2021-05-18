#! /bin/bash
#
gfortran -c -Wall intlib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv intlib.o ~/lib/intlib.o
#
echo "Normal end of execution."
