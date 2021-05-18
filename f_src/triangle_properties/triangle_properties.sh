#! /bin/bash
#
gfortran -c -Wall triangle_properties.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv triangle_properties.o ~/lib/triangle_properties.o
#
echo "Normal end of execution."
