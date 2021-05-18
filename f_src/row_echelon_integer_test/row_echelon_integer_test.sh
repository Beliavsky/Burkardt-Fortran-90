#! /bin/bash
#
gfortran -c -Wall row_echelon_integer_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o row_echelon_integer_test row_echelon_integer_test.o \
  $HOME/lib/row_echelon_integer.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm row_echelon_integer_test.o
#
./row_echelon_integer_test > row_echelon_integer_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm row_echelon_integer_test
#
echo "Normal end of execution."
