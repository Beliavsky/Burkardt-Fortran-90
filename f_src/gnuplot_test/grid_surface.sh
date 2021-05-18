#! /bin/bash
#
gfortran -c -Wall grid_surface.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran grid_surface.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm grid_surface.o
#
mv a.out grid_surface
./grid_surface > grid_surface.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm grid_surface
#
#  Create graphics.
#
gnuplot < grid_surface_commands.txt
#
echo "Normal end of execution."
