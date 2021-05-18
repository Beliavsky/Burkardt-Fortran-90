#! /bin/bash
#
gfortran -c -Wall tet_mesh_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tet_mesh_test.o $HOME/lib/tet_mesh.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tet_mesh_test.o
#
mv a.out tet_mesh_test
./tet_mesh_test > tet_mesh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tet_mesh_test
#
echo "Normal end of execution."
