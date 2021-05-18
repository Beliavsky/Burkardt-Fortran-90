#! /bin/bash
#
gfortran -c -Wall knapsack_01_test.f90
if [ $? -ne 0 ]; then
  echo "Compile error."
  exit
fi
#
gfortran -o knapsack_01_test knapsack_01_test.o $HOME/lib/knapsack_01.o
if [ $? -ne 0 ]; then
  echo "Load error."
  exit
fi
rm knapsack_01_test.o
#
./knapsack_01_test > knapsack_01_test.txt
if [ $? -ne 0 ]; then
  echo "Run error."
  exit
fi
rm knapsack_01_test
#
echo "Normal end of execution."
