#! /bin/bash
#
gfortran -c -Wall asa144.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa144.o ~/lib/asa144.o
#
echo "Normal end of execution."
