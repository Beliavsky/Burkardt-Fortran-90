#! /bin/bash
#
gfortran -c -Wall test_mat_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o test_mat_test test_mat_test.o /$HOME/lib/test_mat.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_mat_test.o
#
./test_mat_test > test_mat_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_mat_test
#
echo "Normal end of execution."
