#! /bin/bash
#
gfortran -c -Wall condition.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv condition.o ~/lib/condition.o
#
echo "Normal end of execution."
