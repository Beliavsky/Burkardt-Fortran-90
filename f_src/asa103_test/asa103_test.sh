#! /bin/bash
#
gfortran -c -Wall asa103_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa103_test asa103_test.o $HOME/lib/asa103.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa103_test.o
#
./asa103_test > asa103_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa103_test
#
echo "Normal end of execution."
