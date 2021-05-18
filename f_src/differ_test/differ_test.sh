#! /bin/bash
#
gfortran -c -Wall differ_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o differ_test differ_test.o $HOME/lib/differ.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm differ_test.o
#
./differ_test > differ_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm differ_test
#
echo "Normal end of execution."
