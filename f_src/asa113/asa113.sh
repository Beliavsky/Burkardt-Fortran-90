#! /bin/bash
#
gfortran -c -Wall asa113.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv asa113.o ~/lib/asa113.o
#
echo "Normal end of execution."
