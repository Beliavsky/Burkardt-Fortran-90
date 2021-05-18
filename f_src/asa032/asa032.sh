#! /bin/bash
#
gfortran -c -Wall asa032.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa032.o ~/lib/asa032.o
#
echo "Normal end of execution."
