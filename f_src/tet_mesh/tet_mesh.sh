#! /bin/bash
#
gfortran -c -Wall tet_mesh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv tet_mesh.o ~/lib/tet_mesh.o
#
echo "Normal end of execution."
