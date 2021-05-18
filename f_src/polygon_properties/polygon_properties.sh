#! /bin/bash
#
gfortran -c -Wall polygon_properties.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polygon_properties.o ~/lib/polygon_properties.o
#
echo "Normal end of execution."
