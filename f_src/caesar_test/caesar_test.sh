#! /bin/bash
#
gfortran -c -Wall caesar_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o caesar_test caesar_test.o $HOME/lib/caesar.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm caesar_test.o
#
./caesar_test > caesar_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm caesar_test
#
echo "Normal end of execution."
