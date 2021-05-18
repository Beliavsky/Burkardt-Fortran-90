#! /bin/bash
#
gfortran -c -Wall isbn_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran isbn_test.o $HOME/lib/isbn.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm isbn_test.o
#
mv a.out isbn_test
./isbn_test > isbn_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm isbn_test
#
echo "Normal end of execution."
