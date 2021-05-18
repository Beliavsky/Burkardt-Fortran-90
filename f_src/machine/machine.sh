#! /bin/bash
#
gfortran -c -Wall machine.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv machine.o ~/lib/machine.o
#
echo "Normal end of execution."
