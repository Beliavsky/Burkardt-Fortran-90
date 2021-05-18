#! /bin/bash
#
gfortran -c -Wall asa266_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa266_test asa266_test.o $HOME/lib/asa266.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa266_test.o
#
./asa266_test > asa266_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa266_test
#
echo "Normal end of execution."
