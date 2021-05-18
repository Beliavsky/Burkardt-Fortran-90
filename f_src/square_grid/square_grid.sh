#! /bin/bash
#
gfortran -c -Wall square_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv square_grid.o ~/lib/square_grid.o
#
echo "Normal end of execution."
