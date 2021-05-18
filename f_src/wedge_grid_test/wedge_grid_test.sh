#! /bin/bash
#
gfortran -c -Wall wedge_grid_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran wedge_grid_test.o $HOME/lib/wedge_grid.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm wedge_grid_test.o
#
mv a.out wedge_grid_test
./wedge_grid_test > wedge_grid_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm wedge_grid_test
#
echo "Normal end of execution."
