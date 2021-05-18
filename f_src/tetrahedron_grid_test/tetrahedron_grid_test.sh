#! /bin/bash
#
gfortran -c -Wall tetrahedron_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran tetrahedron_grid_test.o $HOME/lib/tetrahedron_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm tetrahedron_grid_test.o
#
mv a.out tetrahedron_grid_test
./tetrahedron_grid_test > tetrahedron_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm tetrahedron_grid_test
#
echo "Normal end of execution."
