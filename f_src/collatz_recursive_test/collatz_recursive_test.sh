#! /bin/bash
#
gfortran -c -Wall collatz_recursive_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o collatz_recursive_test collatz_recursive_test.o $HOME/lib/collatz_recursive.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm collatz_recursive_test.o
#
./collatz_recursive_test > collatz_recursive_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm collatz_recursive_test
#
echo "Normal end of execution."
