#! /bin/bash
#
gfortran -c -Wall pyramid_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv pyramid_grid.o ~/lib/pyramid_grid.o
#
echo "Normal end of execution."
