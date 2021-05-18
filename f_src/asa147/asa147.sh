#! /bin/bash
#
gfortran -c -Wall asa147.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa147.o ~/lib/asa147.o
#
echo "Normal end of execution."
