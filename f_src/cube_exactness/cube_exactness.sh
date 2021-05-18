#! /bin/bash
#
gfortran -c -Wall cube_exactness.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv cube_exactness.o ~/lib/cube_exactness.o
#
echo "Normal end of execution."
