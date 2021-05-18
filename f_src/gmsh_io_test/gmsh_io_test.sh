#! /bin/bash
#
gfortran -c -Wall gmsh_io_test.f90
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
#
gfortran -o gmsh_io_test gmsh_io_test.o $HOME/lib/gmsh_io.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm gmsh_io_test.o
#
./gmsh_io_test > gmsh_io_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm gmsh_io_test
#
echo "Normal end of execution."
