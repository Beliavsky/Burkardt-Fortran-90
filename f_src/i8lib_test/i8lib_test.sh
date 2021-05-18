#! /bin/bash
#
gfortran -c -Wall i8lib_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran i8lib_test.o $HOME/lib/i8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm i8lib_test.o
#
mv a.out i8lib_test
./i8lib_test > i8lib_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm i8lib_test
#
echo "Normal end of execution."
