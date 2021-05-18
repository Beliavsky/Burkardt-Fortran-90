#! /bin/bash
#
gfortran -c -Wall asa205.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa205.o ~/lib/asa205.o
#
echo "Normal end of execution."
