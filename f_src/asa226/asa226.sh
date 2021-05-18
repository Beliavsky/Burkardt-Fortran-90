#! /bin/bash
#
gfortran -c -Wall asa226.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa226.o ~/lib/asa226.o
#
echo "Normal end of execution."
