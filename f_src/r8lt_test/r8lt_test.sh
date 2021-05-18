#! /bin/bash
#
gfortran -c -Wall r8lt_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o r8lt_test r8lt_test.o /$HOME/lib/r8lt.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm r8lt_test.o
#
./r8lt_test > r8lt_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm r8lt_test
#
echo "Normal end of execution."
