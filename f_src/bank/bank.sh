#! /bin/bash
#
gfortran -c -Wall bank.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bank.o ~/lib/bank.o
#
echo "Normal end of execution."
