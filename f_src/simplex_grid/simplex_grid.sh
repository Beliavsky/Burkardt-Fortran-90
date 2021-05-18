#! /bin/bash
#
gfortran -c -Wall simplex_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv simplex_grid.o ~/lib/simplex_grid.o
#
echo "Normal end of execution."
