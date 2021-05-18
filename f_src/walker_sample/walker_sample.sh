#! /bin/bash
#
gfortran -c -Wall walker_sample.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv walker_sample.o ~/lib/walker_sample.o
#
echo "Normal end of execution."
