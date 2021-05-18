#! /bin/bash
#
gfortran -c -Wall tetrahedron01_monte_carlo.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tetrahedron01_monte_carlo.o ~/lib/tetrahedron01_monte_carlo.o
#
echo "Normal end of execution."
