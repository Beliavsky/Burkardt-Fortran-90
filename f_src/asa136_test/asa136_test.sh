#! /bin/bash
#
gfortran -c -Wall asa136_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa136_test asa136_test.o $HOME/lib/asa136.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa136_test.o
#
./asa136_test > asa136_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa136_test
#
echo "Normal end of execution."
