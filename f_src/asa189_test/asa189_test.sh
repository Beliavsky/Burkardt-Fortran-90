#! /bin/bash
#
gfortran -c -Wall asa189_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o asa189_test asa189_test.o $HOME/lib/asa189.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm asa189_test.o
#
./asa189_test > asa189_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm asa189_test
#
echo "Normal end of execution."
