#! /bin/bash
#
gfortran -c -Wall quad_mesh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv quad_mesh.o ~/lib/quad_mesh.o
#
echo "Normal end of execution."
