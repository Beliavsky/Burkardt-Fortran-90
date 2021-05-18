#! /bin/bash
#
gfortran -c -Wall quality.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quality.o ~/lib/quality.o
#
echo "Normal end of execution."
