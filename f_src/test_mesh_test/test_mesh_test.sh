#! /bin/bash
#
gfortran -c -Wall test_mesh_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_mesh_test.o $HOME/lib/test_mesh.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_mesh_test.o
#
mv a.out test_mesh_test
./test_mesh_test > test_mesh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_mesh_test
#
echo "Normal end of execution."
