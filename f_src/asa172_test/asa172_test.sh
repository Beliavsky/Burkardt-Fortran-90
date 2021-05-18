#! /bin/bash
#
gfortran -c -Wall asa172_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa172_test asa172_test.o $HOME/lib/asa172.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa172_test.o
#
./asa172_test > asa172_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa172_test
#
echo "Normal end of execution."
