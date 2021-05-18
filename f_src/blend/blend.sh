#! /bin/bash
#
gfortran -c -Wall blend.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv blend.o ~/lib/blend.o
#
echo "Normal end of execution."
