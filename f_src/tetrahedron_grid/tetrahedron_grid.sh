#! /bin/bash
#
gfortran -c -Wall tetrahedron_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron_grid.o ~/lib/tetrahedron_grid.o
#
echo "Normal end of execution."
