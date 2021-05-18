#! /bin/bash
#
gfortran -c -Wall asa007_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa007_test asa007_test.o /$HOME/lib/asa007.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa007_test.o
#
./asa007_test > asa007_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa007_test
#
echo "Normal end of execution."
