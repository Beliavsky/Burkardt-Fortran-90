#! /bin/bash
#
gfortran -c -Wall polygon_triangulate.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv polygon_triangulate.o ~/lib/polygon_triangulate.o
#
echo "Normal end of execution."
