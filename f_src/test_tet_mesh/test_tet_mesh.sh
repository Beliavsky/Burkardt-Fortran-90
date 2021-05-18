#! /bin/bash
#
gfortran -c -Wall test_tet_mesh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_tet_mesh.o ~/lib/test_tet_mesh.o
#
echo "Normal end of execution."
