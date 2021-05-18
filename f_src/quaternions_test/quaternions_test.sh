#! /bin/bash
#
gfortran -c -Wall quaternions_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o quaternions_test quaternions_test.o $HOME/lib/quaternions.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm quaternions_test.o
#
./quaternions_test > quaternions_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm quaternions_test
#
echo "Normal end of execution."
