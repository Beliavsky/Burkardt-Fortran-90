#! /bin/bash
#
gfortran -c -Wall gmsh_io.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv gmsh_io.o ~/lib/gmsh_io.o
#
echo "Normal end of execution."
