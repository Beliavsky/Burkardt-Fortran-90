#! /bin/bash
#
gfortran -c -Wall asa032_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa032_test asa032_test.o /$HOME/lib/asa032.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa032_test.o
#
./asa032_test > asa032_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa032_test
#
echo "Normal end of execution."
