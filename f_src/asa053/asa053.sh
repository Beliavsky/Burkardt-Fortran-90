#! /bin/bash
#
gfortran -c -Wall asa053.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa053.o ~/lib/asa053.o
#
echo "Normal end of execution."
