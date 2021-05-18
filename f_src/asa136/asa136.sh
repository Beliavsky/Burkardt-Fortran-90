#! /bin/bash
#
gfortran -c -Wall asa136.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa136.o ~/lib/asa136.o
#
echo "Normal end of execution."
