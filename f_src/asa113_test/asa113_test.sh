#! /bin/bash
#
gfortran -c -Wall asa113_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa113_test asa113_test.o $HOME/lib/asa113.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa113_test.o
#
./asa113_test > asa113_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa113_test
#
echo "Normal end of execution."
