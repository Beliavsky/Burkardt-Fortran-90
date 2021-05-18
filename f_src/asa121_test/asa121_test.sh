#! /bin/bash
#
gfortran -c -Wall asa121_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa121_test asa121_test.o $HOME/lib/asa121.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa121_test.o
#
./asa121_test > asa121_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa121_test
#
echo "Normal end of execution."
