#! /bin/bash
#
gfortran -c -Wall asa152.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa152.o ~/lib/asa152.o
#
echo "Normal end of execution."
