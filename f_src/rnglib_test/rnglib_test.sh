#! /bin/bash
#
gfortran -c -Wall rnglib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o rnglib_test rnglib_test.o $HOME/lib/rnglib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm rnglib_test.o
#
./rnglib_test > rnglib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm rnglib_test
#
echo "Normal end of execution."
