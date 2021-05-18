#! /bin/bash
#
gfortran -c -Wall cell_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cell_test cell_test.o $HOME/lib/cell.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cell_test.o
#
./cell_test > cell_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cell_test
#
echo "Normal end of execution."
