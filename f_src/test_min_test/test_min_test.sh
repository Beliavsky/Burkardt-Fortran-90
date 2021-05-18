#! /bin/bash
#
gfortran -c -Wall test_min_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o test_min_test test_min_test.o $HOME/lib/test_min.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_min_test.o
#
./test_min_test > test_min_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_min_test
#
echo "Normal end of execution."
