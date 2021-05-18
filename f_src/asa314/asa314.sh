#! /bin/bash
#
gfortran -c -Wall asa314.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa314.o ~/lib/asa314.o
#
echo "Normal end of execution."
