#! /bin/bash
#
gfortran -c -Wall i8lib.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv i8lib.o ~/lib/i8lib.o
#
echo "Normal end of execution."
