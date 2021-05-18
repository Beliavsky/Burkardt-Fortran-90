#! /bin/bash
#
gfortran -c -Wall asa103.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa103.o ~/lib/asa103.o
#
echo "Normal end of execution."
