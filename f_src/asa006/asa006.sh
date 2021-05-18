#! /bin/bash
#
gfortran -c -Wall asa006.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa006.o ~/lib/asa006.o
#
echo "Normal end of execution."
