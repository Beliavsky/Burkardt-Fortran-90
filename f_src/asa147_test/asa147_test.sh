#! /bin/bash
#
gfortran -c -Wall asa147_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa147_test asa147_test.o $HOME/lib/asa147.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa147_test.o
#
./asa147_test > asa147_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa147_test
#
echo "Normal end of execution."
