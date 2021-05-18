#! /bin/bash
#
gfortran -c -Wall triangle_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_io.o ~/lib/triangle_io.o
#
echo "Normal end of execution."
