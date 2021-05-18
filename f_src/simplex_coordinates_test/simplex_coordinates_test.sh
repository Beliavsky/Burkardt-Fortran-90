#! /bin/bash
#
gfortran -c -Wall simplex_coordinates_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran simplex_coordinates_test.o $HOME/lib/simplex_coordinates.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm simplex_coordinates_test.o
#
mv a.out simplex_coordinates_test
./simplex_coordinates_test > simplex_coordinates_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm simplex_coordinates_test
#
echo "Normal end of execution."
