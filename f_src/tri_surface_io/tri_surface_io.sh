#! /bin/bash
#
gfortran -c -Wall tri_surface_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tri_surface_io.o ~/lib/tri_surface_io.o
#
echo "Normal end of execution."
