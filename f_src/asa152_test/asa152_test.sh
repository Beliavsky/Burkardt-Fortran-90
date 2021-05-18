#! /bin/bash
#
gfortran -c -Wall asa152_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa152_test asa152_test.o $HOME/lib/asa152.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa152_test.o
#
./asa152_test > asa152_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa152_test
#
echo "Normal end of execution."
