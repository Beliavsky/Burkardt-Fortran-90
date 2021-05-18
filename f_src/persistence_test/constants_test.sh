#! /bin/bash
#
gfortran -c -Wall constants_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o constants_test constants_test.o /$HOME/lib/constants.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm constants_test.o
#
./constants_test > constants_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm constants_test
#
echo "Normal end of execution."
