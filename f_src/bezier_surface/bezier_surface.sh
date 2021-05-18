#! /bin/bash
#
gfortran -c -Wall bezier_surface.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv bezier_surface.o ~/lib/bezier_surface.o
#
echo "Normal end of execution."
