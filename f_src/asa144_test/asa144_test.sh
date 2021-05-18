#! /bin/bash
#
gfortran -c -Wall asa144_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa144_test asa144_test.o $HOME/lib/asa144.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa144_test.o
#
./asa144_test > asa144_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa144_test
#
echo "Normal end of execution."
