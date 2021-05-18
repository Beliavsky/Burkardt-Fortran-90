#! /bin/bash
#
gfortran -c -Wall test_triangulation_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran test_triangulation_test.o $HOME/lib/test_triangulation.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_triangulation_test.o
#
mv a.out test_triangulation_test
./test_triangulation_test > test_triangulation_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_triangulation_test
#
echo "Normal end of execution."
