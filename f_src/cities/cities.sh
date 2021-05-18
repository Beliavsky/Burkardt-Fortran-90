#! /bin/bash
#
gfortran -c -Wall cities.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cities.o ~/lib/cities.o
#
echo "Normal end of execution."
