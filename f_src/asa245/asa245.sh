#! /bin/bash
#
gfortran -c -Wall asa245.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa245.o ~/lib/asa245.o
#
echo "Normal end of execution."
