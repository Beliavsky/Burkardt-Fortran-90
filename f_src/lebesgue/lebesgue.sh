#! /bin/bash
#
gfortran -c -Wall lebesgue.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv lebesgue.o ~/lib/lebesgue.o
#
echo "Normal end of execution."
