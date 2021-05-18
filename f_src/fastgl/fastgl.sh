#! /bin/bash
#
gfortran -c -Wall fastgl.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv fastgl.o ~/lib/fastgl.o
#
echo "Normal end of execution."
