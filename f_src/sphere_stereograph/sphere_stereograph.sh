#! /bin/bash
#
gfortran -c -Wall sphere_stereograph.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv sphere_stereograph.o ~/lib/sphere_stereograph.o
#
echo "Normal end of execution."
