#! /bin/bash
#
gfortran -c -Wall asa314_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa314_test asa314_test.o $HOME/lib/asa314.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa314_test.o
#
./asa314_test > asa314_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa314_test
#
echo "Normal end of execution."
