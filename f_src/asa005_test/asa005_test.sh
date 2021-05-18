#! /bin/bash
#
gfortran -c -Wall asa005_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa005_test asa005_test.o /$HOME/lib/asa005.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa005_test.o
#
./asa005_test > asa005_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa005_test
#
echo "Normal end of execution."
