#! /bin/bash
#
gfortran -c -Wall asa111.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa111.o ~/lib/asa111.o
#
echo "Normal end of execution."
