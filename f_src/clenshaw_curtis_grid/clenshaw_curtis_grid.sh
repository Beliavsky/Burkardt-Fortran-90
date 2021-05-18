#! /bin/bash
#
gfortran -c -Wall clenshaw_curtis_grid.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv clenshaw_curtis_grid.o ~/lib/clenshaw_curtis_grid.o
#
echo "Normal end of execution."
