#! /bin/bash
#
gfortran -c -Wall asa159.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa159.o ~/lib/asa159.o
#
echo "Normal end of execution."
