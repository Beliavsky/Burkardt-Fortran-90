#! /bin/bash
#
gfortran -c -Wall asa243.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa243.o ~/lib/asa243.o
#
echo "Normal end of execution."
