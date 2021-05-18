#! /bin/bash
#
gfortran -c -Wall asa047.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa047.o ~/lib/asa047.o
#
echo "Normal end of execution."
