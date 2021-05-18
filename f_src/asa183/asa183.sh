#! /bin/bash
#
gfortran -c -Wall asa183.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa183.o ~/lib/asa183.o
#
echo "Normal end of execution."
