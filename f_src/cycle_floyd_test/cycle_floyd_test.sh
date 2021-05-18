#! /bin/bash
#
gfortran -c -Wall cycle_floyd_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o cycle_floyd_test cycle_floyd_test.o $HOME/lib/cycle_floyd.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm cycle_floyd_test.o
#
./cycle_floyd_test > cycle_floyd_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm cycle_floyd_test
#
echo "Normal end of execution."
