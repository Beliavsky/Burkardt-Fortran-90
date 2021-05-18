#! /bin/bash
#
gfortran -c -Wall test_mesh.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
mv test_mesh.o ~/lib/test_mesh.o
#
echo "Normal end of execution."
