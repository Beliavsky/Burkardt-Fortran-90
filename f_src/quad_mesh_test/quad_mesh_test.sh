#! /bin/bash
#
gfortran -c -Wall quad_mesh_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran quad_mesh_test.o $HOME/lib/quad_mesh.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quad_mesh_test.o
#
mv a.out quad_mesh_test
./quad_mesh_test > quad_mesh_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quad_mesh_test
#
ps2png q4_mesh_ex1.eps q4_mesh_ex1.png
ps2png q4_mesh_ex2.eps q4_mesh_ex2.png
#
echo "Normal end of execution."
