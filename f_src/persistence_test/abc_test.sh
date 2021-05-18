#! /bin/bash
#
gfortran -c -Wall abc_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o abc_test abc_test.o /$HOME/lib/abc.o -lm
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm abc_test.o
#
./abc_test > abc_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm abc_test
#
echo "Normal end of execution."
