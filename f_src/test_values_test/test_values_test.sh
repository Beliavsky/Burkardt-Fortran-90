#! /bin/bash
#
gfortran -c -Wall test_values_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o test_values_test test_values_test.o /$HOME/lib/test_values.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm test_values_test.o
#
./test_values_test > test_values_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm test_values_test
#
echo "Normal end of execution."
