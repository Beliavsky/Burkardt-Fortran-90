#! /bin/bash
#
gfortran -c -Wall geometry_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran geometry_test.o $HOME/lib/geometry.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm geometry_test.o
#
mv a.out geometry_test
./geometry_test > geometry_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm geometry_test
#
echo "Normal end of execution."
