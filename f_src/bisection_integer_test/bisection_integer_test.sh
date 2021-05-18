#! /bin/bash
#
gfortran -c -Wall bisection_integer_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o bisection_integer_test bisection_integer_test.o $HOME/lib/bisection_integer.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm bisection_integer_test.o
#
./bisection_integer_test > bisection_integer_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm bisection_integer_test
#
echo "Normal end of execution."
