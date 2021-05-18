#! /bin/bash
#
gfortran -c -Wall correlation.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv correlation.o ~/lib/correlation.o
#
echo "Normal end of execution."
