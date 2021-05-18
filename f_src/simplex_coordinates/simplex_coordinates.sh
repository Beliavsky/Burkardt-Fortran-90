#! /bin/bash
#
gfortran -c -Wall simplex_coordinates.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv simplex_coordinates.o ~/lib/simplex_coordinates.o
#
echo "Normal end of execution."
