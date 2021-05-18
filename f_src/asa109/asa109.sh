#! /bin/bash
#
gfortran -c -Wall asa109.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa109.o ~/lib/asa109.o
#
echo "Normal end of execution."
