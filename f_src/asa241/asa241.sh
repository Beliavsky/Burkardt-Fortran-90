#! /bin/bash
#
gfortran -c -Wall asa241.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa241.o ~/lib/asa241.o
#
echo "Normal end of execution."
