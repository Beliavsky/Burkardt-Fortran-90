#! /bin/bash
#
gfortran -c -Wall sphere_quad.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_quad.o ~/lib/sphere_quad.o
#
echo "Normal end of execution."
