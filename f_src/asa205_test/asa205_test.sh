#! /bin/bash
#
gfortran -c -Wall asa205_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa205_test asa205_test.o $HOME/lib/asa205.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa205_test.o
#
./asa205_test > asa205_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa205_test
#
echo "Normal end of execution."
