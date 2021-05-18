#! /bin/bash
#
gfortran -c -Wall random_data.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv random_data.o ~/lib/random_data.o
#
echo "Normal end of execution."
