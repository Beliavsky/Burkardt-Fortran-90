#! /bin/bash
#
gfortran -c -Wall lcvt.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lcvt.o ~/lib/lcvt.o
#
echo "Normal end of execution."
