#! /bin/bash
#
gfortran -c -Wall asa047_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa047_test asa047_test.o /$HOME/lib/asa047.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa047_test.o
#
./asa047_test > asa047_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa047_test
#
echo "Normal end of execution."
