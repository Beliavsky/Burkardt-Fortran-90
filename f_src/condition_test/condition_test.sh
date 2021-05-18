#! /bin/bash
#
gfortran -c -Wall condition_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o condition_test condition_test.o $HOME/lib/condition.o $HOME/lib/r8lib.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm condition_test.o
#
./condition_test > condition_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm condition_test
#
echo "Normal end of execution."
