#! /bin/bash
#
gfortran -c -Wall dutch_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o dutch_test dutch_test.o $HOME/lib/dutch.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm dutch_test.o
#
./dutch_test > dutch_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm dutch_test
#
echo "Normal end of execution."
